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
;; Shamelessly stolen from: https://github.com/alezost/guix.el/blob/e1dfd96/elisp/guix-prettify.el
(defvar nix-prettify-regexp
  (rx "/" (or "store" "log" (and "nar" (zero-or-one "/gzip")))
      ;; Hash-parts do not include "e", "o", "u" and "t".  See base32Chars
      ;; at <https://github.com/NixOS/nix/blob/f8b84a3/src/libutil/hash.cc>
      "/" (group (= 32 (any "0-9" "a-d" "f-n" "p-s" "v-z"))))
  "Regexp matching file names for prettifying.")

(defun nix-prettify (file-name)
  "Prettify nix store FILE-NAME."
  (replace-regexp-in-string nix-prettify-regexp "…" file-name nil nil 1))

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

;; Shamelessly stolen from `ansible-doc'.
(defun nix-fontify-text (text &optional mode)
  "Add `font-lock-face' properties to TEXT using MODE.

Return a fontified copy of TEXT."
  ;; Graciously inspired by http://emacs.stackexchange.com/a/5408/227
  (if (not (fboundp mode))
      text
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks
        (funcall mode)
        (font-lock-mode))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          ;; Suppress warning about non-interactive use of
          ;; `font-lock-fontify-buffer' in Emacs 25.
          (font-lock-fontify-buffer)))
      ;; Convert `face' to `font-lock-face' to play nicely with font lock
      (goto-char (point-min))
      (while (not (eobp))
        (let ((pos (point)))
          (goto-char (next-single-property-change pos 'face nil (point-max)))
          (put-text-property pos (point) 'font-lock-face
                             (get-text-property pos 'face))))
      (buffer-string))))

(provide 'nix-common)
;;; nix-common.el ends here
