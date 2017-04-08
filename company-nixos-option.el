;;; company-nixos-option.el --- Company backend for NixOS options -*- lexical-binding: t -*-

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
;;     (require 'company-nixos-option)
;;
;;     (with-eval-after-load 'company
;;       (add-to-list 'company-backends 'company-nixos-option))
;;
;; TODO:
;;
;; + [ ] Extract builtins.xml documentation
;;
;; [nix]: https://nixos.org/nix/

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'company)
(require 'nixos-option)

(defvar company-nixos-option-candidates nil)
(defvar company-nixos-option-candidates-loaded-p nil)

(defun company-nixos-option-collect-candidates ()
  "Collect nix candidates from the `nix-options' hash table."
  (or company-nixos-option-candidates-loaded-p
      (setq company-nixos-option-candidates (hash-table-keys (nixos-option-options))
            company-nixos-option-candidates-loaded-p t)))

(defun company-nixos-option-prefix ()
  "Get a company prefix from current position."
  (and (or (looking-at "\\_>") (eq (char-before) ?.))
       (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.") (point)))))

(defun company-nixos-option-candidates (prefix)
  "Return company candidates from PREFIX."
  (company-nixos-option-collect-candidates)
  (all-completions prefix company-nixos-option-candidates))

(defun company-nixos-option-doc-buffer (candidate)
  "Return a company documentation buffer for CANDIDATE."
  (company-doc-buffer (nixos-option-tostring (gethash candidate nixos-option-options))))

(defun company-nixos-option-annotation (candidate)
  "Return a company annotation for CANDIDATE."
  (format "[%s]" (nixos-option-type (gethash candidate nixos-option-options))))

(defun company-nixos-option-location (candidate)
  "Return a company location for CANDIDATE."
  (let ((declarations (nixos-option-declarations (gethash candidate nixos-option-options))))
    (cons (nixos-option-locate-declaration (elt declarations 0)) 1)))

(defun company-nixos-option-meta (candidate)
  "Return a company meta string for CANDIDATE."
  (let ((nix-option (gethash candidate nixos-option-options)))
    (format "%s: %s | %s: %s"
            (propertize "default" 'face 'nix-options-value)
            (nixos-option-display-default nix-option)
            (propertize "example" 'face 'nix-options-value)
            (nixos-option-display-example nix-option))))

;;;###autoload
(defun company-nixos-option (command &optional arg &rest ignored)
  "`company-mode' completion back-end for nix expressions.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nixos-option))
    (prefix (and (derived-mode-p 'nix-mode)
                 (not (company-in-string-or-comment))
                 (or (company-nixos-option-prefix) 'stop)))
    (candidates (company-nixos-option-candidates arg))
    (annotation (company-nixos-option-annotation arg))
    (doc-buffer (company-nixos-option-doc-buffer arg))
    (location (company-nixos-option-location arg))
    (meta (company-nixos-option-meta arg))))

(provide 'company-nixos-option)
;;; company-nixos-option.el ends here
