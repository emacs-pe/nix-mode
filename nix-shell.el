;;; nix-shell.el --- nix-shell integration            -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
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

(defvar nix-shell-process-buffer "*nix-shell*")
(defvar nix-shell-environment-cache (make-hash-table :test 'equal))

(defun nix-shell-locate-root-directory (directory)
  "Locate a project root DIRECTORY for a nix directory."
  (cl-loop for file in nix-shell-files
           when (locate-dominating-file directory file)
           return it))

(defun nix-shell-root (directory)
  "Return a nix sandbox project root from DIRECTORY."
  (or nix-shell-root (nix-shell-locate-root-directory directory)))

(defun nix-shell-start-process (program &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object and ARGS."
  (let ((process (apply #'start-file-process
                        (file-name-nondirectory program)
                        nix-shell-process-buffer program args)))
    (set-process-sentinel process #'nix-shell-process-sentinel)
    process))

(defun nix-shell-process-sentinel (process event)
  "Default sentinel used by `nix-shell-start-process'.

For PROCESS and EVENT."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match-p "^finished" event)
      (message "%s finished" (process-name process)))))

(defun nix-shell-process-filter (process string)
  "PROCESS filter, Insert STRING in process buffer."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (insert string))))

;; TODO(marsam): Allow check environment asynchronously
(defun nix-shell-environment-variables-async (&rest args)
  "Return a sandbox environment variables for ARGS."
  (let ((process (apply #'nix-shell-start-process
                        nix-shell-executable
                        (append '("--run" "printenv") args))))
    (set-process-filter process #'nix-shell-process-filter)
    (set-process-sentinel process
                          (lambda (proc event)
                            (when (memq (process-status proc) '(exit signal))
                              (nix-shell-process-sentinel proc event))
                            (if (and (eq (process-status proc) 'exit)
                                     (= (process-exit-status proc) 0))
                                (message "done")
                              (message "%s failed" (process-name process)))))))

;; FIXME(marsam): merge with the current `exec-path'.
(defun nix-shell-extract-exec-path ()
  "Extract path variable from `process-environment' variable."
  (parse-colon-path (getenv "PATH")))

(defun nix-shell-environment-variables (directory &rest args)
  "Get the environment variables from a nix-shell from DIRECTORY."
  (or (gethash directory nix-shell-environment-cache)
      (puthash directory (let ((default-directory directory))
                           (apply #'process-lines
                                  nix-shell-executable
                                  (append '("--run" "printenv") args)))
               nix-shell-environment-cache)))

;;;###autoload
(defmacro nix-shell-with-shell (directory &rest body)
  "Execute nix-shell DIRECTORY and BODY."
  (declare (indent defun) (debug (body)))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (nix-shell-root (file-name-as-directory (expand-file-name ,directory)))))
       (if ,toplevel
           (let* ((process-environment (nix-shell-environment-variables ,toplevel))
                  (exec-path (nix-shell-extract-exec-path)))
             ,@body)
         (error "Not inside a nix-shell project: %s" default-directory)))))

(provide 'nix-shell)
;;; nix-shell.el ends here
