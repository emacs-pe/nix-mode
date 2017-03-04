;;; nix-shell.el --- nix-shell integration            -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

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
;; TODO:
;;
;; + [ ] Document integration with FlyCheck and related tools

;;; Code:
(eval-when-compile (require 'cl-lib))

(defgroup nix-shell nil
  "nix-shell integration"
  :prefix "nix-shell-"
  :group 'nix-mode)

(defcustom nix-shell-executable "nix-shell"
  "Path to nix-shell executable."
  :type 'string
  :group 'nix-shell)

(defcustom nix-shell-root nil
  "Directory of nix-shell-root.

This variable if set has precedence over `nix-shell-files' lookup."
  :type '(directory :must-match t)
  :safe #'stringp
  :group 'nix-shell)

(defcustom nix-shell-files
  '("shell.nix"
    "default.nix"
    )
  "List of files which be considered to locate the project root.

The topmost match has precedence."
  :type '(repeat string)
  :group 'nix-shell)

(defvar nix-shell-variables-cache (make-hash-table :test 'equal))
(defvar nix-shell-old-process-environment nil
  "The old process environment before the last activate.")
(defvar nix-shell-old-exec-path nil
  "The old exec path before the last activate.")

(cl-defstruct (nix-shell (:constructor nix-shell-new))
  "A structure holding the information of nix-shell."
  exec-path process-environment)

(defmacro nix-shell-with-default-directory (directory &rest body)
  "Set `default-directory' to DIRECTORY and execute BODY."
  (declare (indent defun) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defmacro nix-shell-with-gensyms (symbols &rest body)
  "Bind the SYMBOLS to fresh uninterned symbols and eval BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (cl-gensym (symbol-name ',s))))
                 symbols)
     ,@body))

(defun nix-shell-locate-root-directory (directory)
  "Locate a project root DIRECTORY for a nix directory."
  (cl-loop for file in nix-shell-files
           when (locate-dominating-file directory file)
           return it))

(defun nix-shell-root (directory)
  "Return a nix sandbox project root from DIRECTORY."
  (or nix-shell-root (nix-shell-locate-root-directory directory)))

(defun nix-shell-exec-insert (&rest args)
  "Execute `nix-shell-executable' with ARGS, inserting its output at point."
  (apply #'process-file nix-shell-executable nil (list t nil) nil args))

(defun nix-shell-exec-exit-code (&rest args)
  "Execute `nix-shell-executable' with ARGS, returning its exit code."
  (apply #'process-file nix-shell-executable nil nil nil args))

(defun nix-shell-exec-lines (&rest args)
  "Execute `nix-shell-executable' with ARGS, returning its output as a list of lines."
  (with-temp-buffer
    (apply #'nix-shell-exec-insert args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defun nix-shell-extract-exec-path ()
  "Extract path variable from `process-environment' variable."
  (cl-union (parse-colon-path (getenv "PATH")) exec-path :test 'equal))

;;;###autoload
(defalias 'nix-shell-register #'nix-shell-variables)
;;;###autoload
(defun nix-shell-variables (directory &rest args)
  "Get the environment variables from a nix-shell from DIRECTORY."
  (interactive "Dregister nix-shell: ")
  (or (gethash directory nix-shell-variables-cache)
      (puthash directory (nix-shell-with-default-directory directory
                           ;; XXX: Check if the nix sandbox is ready for consumption
                           (cl-assert (zerop (nix-shell-exec-exit-code "--run" "true")) nil "Nix shell is not available in %s" directory)
                           (let ((process-environment (apply #'nix-shell-exec-lines (append '("--run" "printenv") args))))
                             (nix-shell-new :exec-path (nix-shell-extract-exec-path) :process-environment process-environment)))
               nix-shell-variables-cache)))

;;;###autoload
(defun nix-shell-active (directory)
  "Activate the nix-shell in DIRECTORY."
  (interactive (list (completing-read "Directory: " nix-shell-variables-cache nil t)))
  (if-let ((shell-vars (nix-shell-variables directory)))
      (setq nix-shell-old-exec-path exec-path
            nix-shell-old-process-environment process-environment
            exec-path (nix-shell-exec-path shell-vars)
            process-environment (nix-shell-process-environment shell-vars))
    (error "Not inside a nix-shell project: %s" directory)))

;;;###autoload
(defun nix-shell-deactivate ()
  "Disable nix-shell."
  (interactive)
  (and nix-shell-old-exec-path
       (setq exec-path nix-shell-old-exec-path
             nix-shell-old-exec-path nil))
  (and nix-shell-old-process-environment
       (setq process-environment nix-shell-old-process-environment
             nix-shell-old-process-environment nil)))

;;;###autoload
(defmacro nix-shell-with-shell (directory &rest body)
  "Execute nix-shell DIRECTORY and BODY."
  (declare (indent defun) (debug (body)))
  (nix-shell-with-gensyms (shell-root shell-vars)
    `(if-let ((,shell-root (nix-shell-root ,(expand-file-name directory)))
              (,shell-vars (nix-shell-variables ,shell-root)))
         (let* ((exec-path (nix-shell-exec-path ,shell-vars))
                (process-environment (nix-shell-process-environment ,shell-vars)))
           ,@body)
       (error "Not inside a nix-shell project: %s" default-directory))))

(provide 'nix-shell)
;;; nix-shell.el ends here
