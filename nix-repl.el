;;; nix-repl.el --- nix-repl integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

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

;; See: <URL:https://github.com/edolstra/nix-repl>

;; TODO:
;;
;; + [ ] Add completion support.
;; + [ ] nix-repl was included in update to nix 1.12. https://github.com/edolstra/nix-repl/commit/ad0c46b

;;; Code:
(require 'comint)

(defvar compilation-error-regexp-alist)

(defgroup nix-repl nil
  "Nix-Repl integration."
  :prefix "nix-repl-"
  :group 'nix-mode)

(defcustom nix-repl-executable "nix-repl"
  "Path to nix-repl executable path."
  :type '(file :must-match t)
  :group 'nix-repl)

(defcustom nix-repl-history-file (expand-file-name "~/.nix-repl-history")
  "Path to nix-repl executable path."
  :type '(file :must-match t)
  :group 'nix-repl)

(defvar nix-repl-process-buffer-name "*nix-repl*")
(defconst nix-repl-cli-prompt-regexp "^nix-repl> ")

(defcustom nix-repl-compilation-regexp-alist
  `((,(rx "error: " (+ not-newline) " at " (? "\"") (group (minimal-match (+ not-newline))) (? "\"") ":" (group (+ num)) ":" (group (+ num)))
     1 2 3))
  "`compilation-error-regexp-alist' for nix instantiate errors."
  :type '(alist string)
  :group 'nix-repl)

(defun nix-repl-get-process ()
  "Return inferior nix-repl process for current buffer."
  (get-buffer-process (if (derived-mode-p 'nix-repl-cli-mode) (current-buffer) nix-repl-process-buffer-name)))

(defun nix-repl-get-process-or-error (&optional interactivep)
  "Return inferior nix-repl process for current buffer or signal error.

When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (nix-repl-get-process)
      (if interactivep
          (user-error "Start a Nix-repl process first with `M-x run-nix' or `%s'" (key-description (where-is-internal #'run-nix overriding-local-map t)))
        (error "No inferior nix-repl process running"))))

(defun nix-repl-send-string (string &optional process msg)
  "Send STRING to inferior nix-repl PROCESS.

When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list (read-string "Nix command: ") nil t))
  (let ((process (or process (nix-repl-get-process-or-error msg))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

(defvar nix-repl-output-filter-in-progress nil)
(defvar nix-repl-output-filter-buffer nil)

(defun nix-repl-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT is ends with input prompt."
  (string-match
   ;; XXX: It seems on macOS an extra carriage return is attached
   ;; at the end of output, this handles that too.
   (concat
    "\r?\n?"
    ;; Remove initial caret from calculated regexp
    (replace-regexp-in-string (rx string-start ?^) "" nix-repl-cli-prompt-regexp)
    (rx eos))
   output))

(defun nix-repl-output-filter (string)
  "Filter used in `nix-repl-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`nix-repl-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   nix-repl-output-filter-buffer
   (concat nix-repl-output-filter-buffer string))
  (when (nix-repl-comint-end-of-output-p
         nix-repl-output-filter-buffer)
    ;; Output ends when `nix-repl-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq nix-repl-output-filter-in-progress nil
          nix-repl-output-filter-buffer
          (substring nix-repl-output-filter-buffer
                     0 (match-beginning 0))))
  "")

(defun nix-repl-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or process (nix-repl-get-process-or-error)))
        (comint-preoutput-filter-functions
         '(nix-repl-output-filter))
        (nix-repl-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (nix-repl-send-string string process)
       (while nix-repl-output-filter-in-progress
         ;; `nix-repl-output-filter' takes care of setting
         ;; `nix-repl-output-filter-in-progress' to NIL after it detects end of
         ;; output.
         (accept-process-output process))
       (prog1 nix-repl-output-filter-buffer
         (setq nix-repl-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))

(define-derived-mode nix-repl-cli-mode comint-mode "nix-repl"
  "Major mode for `nix-repl-cli'.

\\<nix-repl-cli-mode-map>"
  (setq-local comint-process-echoes t)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp nix-repl-cli-prompt-regexp)
  (setq-local comint-input-ring-file-name nix-repl-history-file)
  (comint-read-input-ring t)
  (setq-local compilation-error-regexp-alist nix-repl-compilation-regexp-alist)
  (define-key nix-repl-cli-mode-map "\t" 'completion-at-point)
  (compilation-shell-minor-mode 1))

;;;###autoload
(defalias 'run-nix #'nix-repl)

;;;###autoload
(defun nix-repl ()
  "Run nix-repl process."
  (interactive)
  (with-current-buffer (make-comint-in-buffer "nix-repl"
                                              nix-repl-process-buffer-name
                                              nix-repl-executable)
    (nix-repl-cli-mode)
    (pop-to-buffer (current-buffer) t)))

(provide 'nix-repl)
;;; nix-repl.el ends here
