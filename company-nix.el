;;; company-nix.el --- Company backend for nix expressions -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (company "0.8.0"))

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
;; A company backend for [nix][] expressions.
;;
;; Setup:
;;
;; Add to your `init.el':
;;
;;     (require 'company-nix)
;;
;;     (with-eval-after-load 'company
;;       (add-to-list 'company-backends 'company-nix))
;;
;; TODO:
;;
;; + [ ] Extract builtins.xml documentation
;;
;; [nix]: https://nixos.org/nix/

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'company)
(require 'nix-options)

(defvar company-nix-candidates nil)
(defvar company-nix-candidates-loaded-p nil)

(defun company-nix-collect-candidates ()
  "Collect nix candidates from the `nix-options' hash table."
  (or company-nix-candidates-loaded-p
      (setq company-nix-candidates (hash-table-keys (nix-options))
            company-nix-candidates-loaded-p t)))

(defun company-nix-prefix ()
  "Get a company prefix from current position."
  (and (or (looking-at "\\_>") (eq (char-before) ?.))
       (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.") (point)))))

(defun company-nix-candidates (prefix)
  "Return company candidates from PREFIX."
  (company-nix-collect-candidates)
  (all-completions prefix company-nix-candidates))

(defun company-nix-doc-buffer (candidate)
  "Return a company documentation buffer for CANDIDATE."
  (company-doc-buffer (nix-options-option-string (gethash candidate nix-options))))

(defun company-nix-annotation (candidate)
  "Return a company annotation for CANDIDATE."
  (format "[%s]" (nix-options-option-type (gethash candidate nix-options))))

(defun company-nix-location (candidate)
  "Return a company location for CANDIDATE."
  (let ((declarations (nix-options-option-declarations (gethash candidate nix-options))))
    (cons (nix-options-locate-declaration (elt declarations 0)) 1)))

(defun company-nix-meta (candidate)
  "Return a company meta string for CANDIDATE."
  (let ((nix-option (gethash candidate nix-options)))
    (format "%s: %s | %s: %s"
            (propertize "default" 'face 'nix-options-value)
            (nix-options-display-default nix-option)
            (propertize "example" 'face 'nix-options-value)
            (nix-options-display-example nix-option))))

;;;###autoload
(defun company-nix (command &optional arg &rest ignored)
  "`company-mode' completion back-end for nix expressions.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nix))
    (prefix (and (derived-mode-p 'nix-mode)
                 (not (company-in-string-or-comment))
                 (or (company-nix-prefix) 'stop)))
    (candidates (company-nix-candidates arg))
    (annotation (company-nix-annotation arg))
    (doc-buffer (company-nix-doc-buffer arg))
    (location (company-nix-location arg))
    (meta (company-nix-meta arg))))

(provide 'company-nix)
;;; company-nix.el ends here
