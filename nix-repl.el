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
