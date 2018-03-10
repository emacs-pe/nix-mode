;;; nix-shell.el --- nix-shell integration            -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/nix-mode
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; nix-shell integration.  See: <URL:https://nixos.org/nix/manual/#sec-nix-shell>
;;
;; Usage:
;;
;; The main entry points are `nix-shell-activate', which queries the user for a
;; "shell.nix" path to activate, and `nix-shell-deactivate' to deactivate it.
;;
;; `nix-shell' will start a inferior `shell' with nix-shell.

;;; Code:
(eval-when-compile (require 'cl-lib))

(defgroup nix-shell nil
  "nix-shell integration."
  :prefix "nix-shell-"
  :group 'nix-mode)

(defcustom nix-shell-executable "nix-shell"
  "Path to nix-shell executable."
  :type 'string
  :group 'nix-shell)

(defcustom nix-shell-files
  '("shell.nix"
    "default.nix"
    )
  "List of files which be considered to locate the project root.

The topmost match has precedence."
  :type '(repeat string)
  :group 'nix-shell)

(defvar nix-shell-buffer-name "*nix-shell*")
(defvar nix-shell-stderr-file (make-temp-file "nix-shell")
  "File writing nix-shell command standard error.")
(defvar nix-shell-variables-cache (make-hash-table :test 'equal)
  "Hash table holding the cached nix-shell variables.")
(defvar nix-shell-old-process-environment nil
  "The old process environment before the last activate.")
(defvar nix-shell-old-exec-path nil
  "The old exec path before the last activate.")
(defvar nix-shell-command-history nil
  "Variable for history of nix-shell-command.")

(defvar nix-shell-pre-activate-hooks nil
  "Hooks run before a nix-shell is activated.")
(defvar nix-shell-post-activate-hooks nil
  "Hooks run after a nix-shell is activated.")
(defvar nix-shell-pre-deactivate-hooks nil
  "Hooks run before a nix-shell is deactivated.")
(defvar nix-shell-post-deactivate-hooks nil
  "Hooks run after a nix-shell is deactivated.")

(defmacro nix-shell-with-gensyms (symbols &rest body)
  "Bind the SYMBOLS to fresh uninterned symbols and eval BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (cl-gensym (symbol-name ',s))))
                 symbols)
     ,@body))

(cl-defstruct (nix-shell (:constructor nix-shell-new))
  "A structure holding the information of nix-shell."
  exec-path process-environment)

(defun nix-shell-file-contents (file)
  "Return the contents of FILE, as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nix-shell-exec-insert (program &rest args)
  "Execute PROGRAM with ARGS, inserting its output at point."
  (apply #'call-process program nil (list t nix-shell-stderr-file) nil args))

(autoload 'ansi-color-filter-apply "ansi-color")

(defun nix-shell-exec-success (program &rest args)
  "Execute PROGRAM with ARGS, returning t if its exit code is 0."
  (or (zerop (apply #'nix-shell-exec-insert program args))
      ;; TODO(marsam): is there a better way to do this
      (error (ansi-color-filter-apply (nix-shell-file-contents nix-shell-stderr-file)))))

(defun nix-shell-exec-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines."
  (with-temp-buffer
    (apply #'nix-shell-exec-insert program args)
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" 'omit-nulls)))

(cl-defun nix-shell-locate-expression (&optional (directory default-directory))
  "Locate a nix-shell expression from `nix-shell-files' for a DIRECTORY."
  (cl-loop for file in nix-shell-files
           when (locate-dominating-file directory file)
           return (expand-file-name file it)))

(defun nix-shell-read-args (prompt)
  "Read nix-shell args with PROMPT."
  (and current-prefix-arg (split-string-and-unquote (read-shell-command prompt nil 'nix-shell-command-history))))

(defun nix-shell-read-path (prompt)
  "Read nix-shell expression path with PROMPT."
  (let ((path (nix-shell-locate-expression)))
    (or (and (not current-prefix-arg) path)
        (read-file-name prompt nil nil t path))))

(defun nix-shell-extract-exec-path ()
  "Add paths from PATH environment variable to `exec-path'.  Does not modify `exec-path'."
  (cl-loop for path in (parse-colon-path (getenv "PATH"))
           with exec-path = exec-path
           do (add-to-list 'exec-path path)
           finally return exec-path))

(defun nix-shell-variables (path &rest args)
  "Get the environment variables from a nix-shell expression PATH.

ARGS are passed to nix-shell executable to generate the nix shell
environment."
  (or (gethash path nix-shell-variables-cache)
      (puthash path (let ((default-directory (file-name-directory path)))
                      (apply #'nix-shell-exec-success nix-shell-executable (append  args (list path "--run" "true")))
                      (let ((process-environment (apply #'nix-shell-exec-lines nix-shell-executable (append args (list path "--run" "printenv")))))
                        (nix-shell-new :exec-path (nix-shell-extract-exec-path) :process-environment process-environment)))
               nix-shell-variables-cache)))

;;;###autoload
(defun nix-shell-invalidate-cache ()
  "Invalidate nix-shell variables cache."
  (interactive)
  (clrhash nix-shell-variables-cache))

;;;###autoload
(defun nix-shell-activate (path &rest args)
  "Activate the nix-shell for expression PATH with ARGS."
  (interactive (cons (nix-shell-read-path "nix expression path: ")
                     (nix-shell-read-args "nix-shell extra args: ")))
  (nix-shell-deactivate)
  (let ((shell-vars (apply #'nix-shell-variables (expand-file-name path) args)))
    (run-hooks 'nix-shell-pre-activate-hooks)
    (setq nix-shell-old-exec-path exec-path
          nix-shell-old-process-environment process-environment
          exec-path (nix-shell-exec-path shell-vars)
          process-environment (nix-shell-process-environment shell-vars))
    (run-hooks 'nix-shell-post-activate-hooks)))

;;;###autoload
(defun nix-shell-deactivate ()
  "Deactivate nix-shell."
  (interactive)
  (run-hooks 'nix-shell-pre-deactivate-hooks)
  (and nix-shell-old-exec-path
       (setq exec-path nix-shell-old-exec-path
             nix-shell-old-exec-path nil))
  (and nix-shell-old-process-environment
       (setq process-environment nix-shell-old-process-environment
             nix-shell-old-process-environment nil))
  (run-hooks 'nix-shell-post-deactivate-hooks))

;;;###autoload
(defmacro nix-shell-with-shell (path &rest body)
  "Activate nix-shell from PATH and evaluate BODY."
  (declare (indent defun) (debug (body)))
  (nix-shell-with-gensyms (shell-vars)
    `(let* ((,shell-vars (nix-shell-variables (expand-file-name ,path)))
            (exec-path (nix-shell-exec-path ,shell-vars))
            (process-environment (nix-shell-process-environment ,shell-vars)))
       ,@body)))

(defvar explicit-nix-shell-args)
(defvar explicit-shell-file-name)

;; NB: In a future honor `NIX_BUILD_SHELL' when start nix-shell. See:
;;     gh:NixOS/nix#730, gh:NixOS/nix#498, gh:NixOS/nix#777
;;;###autoload
(defun nix-shell (&rest args)
  "Run an inferior nix-shell with ARGS."
  (interactive (nix-shell-read-args "nix-shell args: "))
  (let ((explicit-shell-file-name nix-shell-executable)
        (explicit-nix-shell-args args))
    (shell nix-shell-buffer-name)))

(provide 'nix-shell)
;;; nix-shell.el ends here
