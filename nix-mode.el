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
;; + [ ] Indentation with `smie'.  See: https://github.com/NixOS/nix/blob/master/doc/manual/nix-lang-ref.xml
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
  `(,(regexp-opt '("else" "if" "import" "in" "inherit" "let" "or" "rec" "then" "with") 'symbols)

    ;; Builtins
    (,(rx symbol-start
          (zero-or-one "builtins.")
          (or
           "add" "all" "any" "attrNames" "attrValues" "baseNameOf"
           "compareVersions" "concatLists" "currentSystem" "currentTime"
           "deepSeq" "derivation" "dirOf" "div" "elem" "elemAt" "fetchurl"
           "filter" "filterSource" "foldl’" "fromJSON" "functionArgs" "genList"
           "getAttr" "getEnv" "hasAttr" "hashString" "head" "intersectAttrs"
           "isAttrs" "isBool" "isFunction" "isInt" "isList" "isNull" "isString"
           "langVersion" "length" "lessThan" "listToAttrs" "map" "match" "mul"
           "nixVersion" "parseDrvName" "pathExists" "readDir" "readFile"
           "removeAttrs" "replaceStrings" "scopedImport" "seq" "sort" "storeDir"
           "storePath" "stringLength" "sub" "substring" "tail" "toFile" "toJSON"
           "toPath" "toString" "toXML" "trace" "typeOf"
           )
          symbol-end) . font-lock-builtin-face)
    (,(regexp-opt '("builtins") 'symbols) . font-lock-builtin-face)

    (,(regexp-opt '("assert" "abort" "throw") 'symbols) . font-lock-warning-face)

    ;; Constants
    (,(regexp-opt '("true" "false" "null") 'symbols) . font-lock-constant-face)

    ;; Imports. i.e ./some/file.nix and <nixpkgs>
    (,(rx symbol-start "import" (1+ space) (group (1+ (in alnum "<>+./-"))) symbol-end)
     (1 font-lock-function-name-face))

    ;; Urls.
    (,(rx symbol-start (group alpha (0+ (in alnum "+.-")) ":" (1+ (in alnum punct))) symbol-end)
     (1 font-lock-string-face))

    ;; Assignments
    (,(rx symbol-start (group (in alpha "_") (zero-or-more (in alnum "_.'-"))) (zero-or-more space) "=")
     (1 font-lock-variable-name-face)))
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

(defconst nix--prettify-symbols-alist
  '((">=" . ?≥)
    ("<=" . ?≤))
  "Alist of symbol prettifications for Nix.")

(defconst nix-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (group "''"))
    (0 (ignore (nix-syntax-stringify))))))

;;; Shamelessly stolen from `python.el'.
(defsubst nix-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 2).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 2)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun nix-syntax-stringify ()
  "Put `syntax-table' property correctly on single/double quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (nix-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

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
  (setq-local prettify-symbols-alist nix--prettify-symbols-alist)
  (setq-local syntax-propertize-function nix-syntax-propertize-function)
  (setq-local indent-line-function 'nix-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(provide 'nix-mode)
;;; nix-mode.el ends here
