;;; nix-common.el --- nix common                      -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

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
(defmacro nix-with-default-directory (directory &rest body)
  "Set `default-directory' to DIRECTORY and execute BODY."
  (declare (indent defun) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defmacro nix-with-gensyms (symbols &rest body)
  "Bind the SYMBOLS to fresh uninterned symbols and eval BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (cl-gensym (symbol-name ',s))))
                 symbols)
     ,@body))

(defun nix-exec-insert (program &rest args)
  "Execute PROGRAM with ARGS, inserting its output at point."
  (apply #'process-file program nil (list t nil) nil args))

(defun nix-exec-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines."
  (with-temp-buffer
    (apply #'nix-exec-insert program args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defun nix-exec-string (program &rest args)
  "Execute PROGRAM with ARGS, returning the first line of its output."
  (with-temp-buffer
    (apply #'nix-exec-insert program args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun nix-exec-exit-code (program &rest args)
  "Execute PROGRAM with ARGS, returning its exit code."
  (apply #'process-file program nil nil nil args))

(defun nix-exec-success (program &rest args)
  "Execute PROGRAM with ARGS, returning t if its exit code is 0."
  (= (apply #'nix-exec-exit-code program args) 0))

(provide 'nix-common)
;;; nix-common.el ends here
