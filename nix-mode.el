;;; nix-mode.el --- Major mode for nix expressions    -*- lexical-binding: t -*-

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
;; A major mode for [Nix][] expressions.
;;
;; ![screenshot](https://github.com/emacs-pe/nix-mode/raw/master/assets/screenshot.png)
;;
;; TODO:
;;
;; + [ ] Multiline strings
;; + [ ] Indentation with `smie'. See: https://github.com/NixOS/nix/blob/master/doc/manual/nix-lang-ref.xml
;;
;; [Nix]: https://nixos.org/nix/

;;; Code:
(require 'smie)

(defgroup nix-mode nil
  "Major mode for editing Nix expressions"
  :prefix "nix-mode-"
  :group 'languages)

(defcustom nix-indent-offset 2
  "Basic size of one indentation step."
  :type 'integer
  :safe 'integerp
  :group 'nix-mode)

(defvar nix-font-lock-keywords
  ;; keywords
  `(,(rx symbol-start
         (or
          "if" "then" "else" "with" "let" "in" "rec" "inherit" "or"
          "builtins" "baseNameOf" "derivation" "dirOf" "fetchTarball"
          "import" "isNull" "map" "removeAttrs" "toString")
         symbol-end)

    (,(regexp-opt '("assert" "abort" "throw") 'symbols) . font-lock-warning-face)

    ;; Constants
    (,(regexp-opt '("true" "false" "null") 'symbols) . font-lock-constant-face)

    ;; Urls.
    (,(rx alpha (zero-or-more (in alnum "+.-")) ":" (one-or-more (in alnum punctuation))) . font-lock-constant-face)

    ;; Angle brackets path: i.e. <nixpkgs>
    (,(rx "<" (one-or-more (in alnum "+.-")) (zero-or-more "/" (in alnum "+.-")) ">") . font-lock-constant-face)

    ;; Assignments
    (,(rx symbol-start (group (in alpha "_") (zero-or-more (in alnum "_.'-") )) (zero-or-more space) "=") 1 font-lock-variable-name-face))
  "Font lock keywords for nix.")

(defvar nix-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments.
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; Python-style comments.
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" table)
    ;; Blocks.
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    ;; Lists.
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Nix mode.")

;; FIXME: replace this with smie support
(defun nix-indent-line ()
  "Indent current line in a Nix expression."
  (interactive)
  (indent-relative 'first-only))

;;;###autoload
(define-derived-mode nix-mode prog-mode "Nix"
  "Major mode for editing Nix expressions.

\\{nix-mode-map}"
  (setq font-lock-defaults '(nix-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local indent-line-function 'nix-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(provide 'nix-mode)
;;; nix-mode.el ends here
