;;; nix.el --- Nix functionality                      -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;; Common utilities for `nix-mode'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'tramp)

(defface nix-section
  '((t :inherit (bold font-lock-function-name-face)))
  "Face used on section names in package description buffers."
  :group 'nix)

(defface nix-value
  '((t :weight bold))
  "Face for nix-package values descriptions."
  :group 'nix-package)

(defface nix-not-given
  '((t :inherit font-lock-comment-face))
  "Face for not given values examples."
  :group 'nix)

;; Shamelessly stolen from: https://github.com/alezost/guix.el/blob/e1dfd96/elisp/guix-prettify.el
(defvar nix-prettify-regexp
  (rx "/" (or "store" "log" (and "nar" (zero-or-one "/gzip")))
      ;; Hash-parts do not include "e", "o", "u" and "t".  See base32Chars
      ;; at <https://github.com/NixOS/nix/blob/f8b84a3/src/libutil/hash.cc>
      "/" (group (= 32 (any "0-9" "a-d" "f-n" "p-s" "v-z"))))
  "Regexp matching file names for prettifying.")

(defun nix-button-browse-url (button)
  "Open web browser on page pointed to by BUTTON target property."
  (browse-url (button-get button 'target)))

(define-button-type 'nix-browse-url
  'action #'nix-button-browse-url
  'help-echo "mouse-2, RET: goto homepage")

(defun nix-link-button (url &optional name)
  "Create a button for URL with NAME."
  (make-text-button (or name url) nil 'type 'nix-browse-url 'target url))

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

(defsubst nix-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defsubst nix-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol (symbol-name string-or-symbol)))

(defsubst nix-keyword-to-string (keyword)
  "Convert a KEYWORD to a symbol, by removing leading colon (:) character."
  (if (keywordp keyword) (substring (symbol-name keyword) 1) keyword))

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

(defun nix-exec (program &rest args)
  "Execute PROGRAM with ARGS in a compilation buffer."
  (compilation-start (mapconcat #'shell-quote-argument (cons program (delq nil args)) " ")))

(defsubst nix-goto-line (n)
  "Go to line N."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- n))))

(defsubst nix-attribute-to-package (attribute)
  "Return a package name from an nix ATTRIBUTE."
  (string-trim-left attribute (regexp-opt '("nixos." "nixpkgs."))))

(defsubst nix-join-lines (&rest strings)
  "Join all STRINGS using newlines."
  (mapconcat 'identity strings "\n"))

(defun nix-indent-string (string &optional length)
  "Indent STRING to LENGTH from line start."
  (replace-regexp-in-string "^" (make-string (or length 4) 32) string))

(defun nix-format-properties (&rest properties)
  "Return an string from PROPERTIES."
  (cl-loop for (key value) on properties by #'cddr
           nconc (list (propertize (upcase (nix-keyword-to-string key)) 'face 'nix-section)
                       (nix-indent-string (or value (propertize "Not specified" 'face 'nix-not-given))))
           into output
           finally return (apply #'nix-join-lines output)))

(defun nix-insert-format (buffer-or-name &rest properties)
  "Insert into BUFFER-OR-NAME a formatted plist PROPERTIES."
  (declare (indent 1))
  (with-help-window (get-buffer-create buffer-or-name)
    (with-current-buffer standard-output
      (insert (apply #'nix-format-properties properties)))))

(cl-defun nix-login-name (&optional (file default-directory))
  "Return the name under which the user accesses the given FILE."
  (or (and (file-remote-p file)
           ;; tramp case: execute "whoami" via tramp
           (let ((default-directory (file-name-directory file))
                 process-file-side-effects)
             (nix-exec-string "whoami")))
      ;; normal case
      (user-login-name)
      ;; if user-login-name is nil, return the UID as a string
      (number-to-string (user-uid))))

(cl-defun nix-tramp-buffer-name (name &optional (directory default-directory))
  "Return a shell buffer NAME for DIRECTORY."
  (if (file-remote-p directory)
      (let ((vec (tramp-dissect-file-name directory)))
        (if (fboundp 'tramp-file-name-real-host) ; Old versions of TRAMP
            (let ((user (tramp-file-name-user vec))
                  (host (tramp-file-name-real-host vec)))
              (if (zerop (length user)) (format "*%s/%s*" name host) (format "*%s/%s@%s*" name user host)))
          (let ((user-domain (tramp-file-name-user-domain vec))
                (host-port (tramp-file-name-host-port vec)))
            (if (zerop (length user-domain)) (format "*%s/%s*" name host-port) (format "*%s/%s@%s*" name user-domain host-port)))))
    (format "*%s*" name)))

(cl-defun nix-tramp-file-relative (filename &optional (directory default-directory))
  "Return a tramp-aware for FILENAME in DIRECTORY."
  (if (file-remote-p directory)
      (let ((vec (tramp-dissect-file-name directory)))
        (condition-case nil           ; New `tramp-file-name' since Emacs26.1
            (tramp-make-tramp-file-name
             (tramp-file-name-method vec)
             (tramp-file-name-user vec)
             (tramp-file-name-domain vec)
             (tramp-file-name-host vec)
             (tramp-file-name-port vec)
             filename
             (tramp-file-name-hop vec))
          (wrong-number-of-arguments
           (with-no-warnings
             (tramp-make-tramp-file-name
              (tramp-file-name-method vec)
              (tramp-file-name-user vec)
              (tramp-file-name-host vec)
              filename
              (tramp-file-name-hop vec))))))
    filename))

(defun nix-find-file-relative (filename)
  "Edit the existing file tramp-aware FILENAME."
  (find-file-existing (nix-tramp-file-relative filename)))

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

;; Shamelessly stolen from: https://github.com/magnars/s.el/blob/fc395c8/s.el#L445-L462
(defun nix-regexp-match (regexp string &optional start)
  "Retrieve the match of REGEXP against a matching STRING.

Behaves like JavaScript's String.prototype.match.  When the given
expression matches the string, this function returns a list of
the whole matching string and a string for each matched
sub-expressions.  If it did not match the returned value is an
empty list (nil).

When START is non-nil the search will start at that index."
  (save-match-data
    (if (string-match regexp string start)
        (let ((match-data-list (match-data))
              result)
          (while match-data-list
            (let* ((beg (car match-data-list))
                   (end (cadr match-data-list))
                   (subs (if (and beg end) (substring string beg end) nil)))
              (setq result (cons subs result))
              (setq match-data-list
                    (cddr match-data-list))))
          (nreverse result)))))

(provide 'nix)
;;; nix.el ends here
