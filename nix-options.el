;;; nix-options.el --- Nix options                    -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.1"))

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

;; See: https://nixos.org/nixos/options.html

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist))

(require 'pcase)
(require 'json)

(declare-function nix-mode "nix-mode" nil)

(defgroup nix-options nil
  "Interface for NixOS options."
  :prefix "nix-options-"
  :group 'languages)

(defcustom nix-options-file nil
  "Nix options file."
  :type '(file :must-match t)
  :group 'nix-options)

(defcustom nix-options-from-homepage-p nil
  "Whether to use nix-options file from homepage."
  :type 'booleanp
  :group 'nix-options)

(defface nix-options-value
  '((t :weight bold))
  "Face for nix-options values descriptions."
  :group 'nix-options)

(defface nix-options-not-given
  '((t :inherit font-lock-comment-face))
  "Face for not given values examples."
  :group 'nix-options)

(defvar nix-options-loaded-p nil)
(defvar nix-options-indent-default 4)
(defvar nix-options-empty-string "not given")
(defvar nix-options-show-history nil)
(defvar nix-options-show-buffer-name "*NixOS option*")

(defvar nix-options (make-hash-table :test 'equal))

(cl-defstruct (nix-options-option
               (:constructor nil)
               (:copier nil)
               (:constructor nix-options-option-new)
               (:type vector))
  "A structure holding all the information of a NixOS option."
  name declarations default description example readOnly type)

(defconst nix-options-json-url "https://nixos.org/nixos/options.json.gz"
  "Url for nix options from the nixos homepage.")

(defun nix-options-collect (json-file)
  "Collect nix candidates from the JSON-FILE."
  (seq-do (lambda (item)
            (let-alist (cdr item)
              (puthash (symbol-name (car item))
                       (nix-options-option-new
                        :name (symbol-name (car item))
                        :default .default
                        :declarations .declarations
                        :description .description
                        :example .example
                        :readOnly .readOnly
                        :type .type)
                       nix-options)))
          json-file))

(defun nix-options-generate-json-options ()
  "Generate json options."
  (let* ((process-environment (append '("LC_ALL=C" "NIXPKGS_ALLOW_UNFREE=1")
                                      process-environment))
         (command "nix-build -Q --no-out-link '<nixpkgs/nixos/release.nix>' -A options")
         ;; FIXME: Check exit code
         (outlines (split-string (shell-command-to-string command) "\n" 'omit-nulls))
         (directory (car (last outlines))))
    (expand-file-name "share/doc/nixos/options.json" directory)))

(defun nix-options-http-fetch ()
  "Fetch nix options from NixOS homepage."
  (let ((file-name (locate-user-emacs-file "nix-options.json")))
    (url-copy-file nix-options-json-url file-name 'ok-if-already-exists)
    file-name))

(defun nix-options-file (&optional from-homepage)
  "Get nix-options-file or generate one.

If FROM-HOMEPAGE is non nil, will download the json options from
the official website."
  (unless (and nix-options-file (file-readable-p nix-options-file))
    (message "Generating nix-options file... ")
    (let ((file (if from-homepage
                    ;; TODO: if this fails to generate nix-options offer to use
                    ;;       nix-options from the homepage
                    (nix-options-http-fetch)
                  (nix-options-generate-json-options))))
      (if (yes-or-no-p "Save the `nix-options-file' for future sessions? ")
          (customize-save-variable 'nix-options-file file)
        (customize-set-variable 'nix-options-file file))))
  nix-options-file)

(defun nix-options (&optional from-homepage)
  "Populate `nix-options'.

If FROM-HOMEPAGE is non nil will download options file from nixos.org."
  (unless nix-options-loaded-p
    (message "loading NixOS options...")
    (setq nix-options-loaded-p t)
    (nix-options-collect (json-read-file (nix-options-file (or from-homepage nix-options-from-homepage-p)))))
  nix-options)

(defun nix-options-locate-declaration (file-name)
  "Locate a nix declaration for nix FILE-NAME."
  ;; FIXME: check exit-code
  (let ((command (combine-and-quote-strings (list "nix-instantiate" "--find-file" (concat "nixpkgs/" file-name)))))
    (string-trim (shell-command-to-string command))))

(defun nix-options-display-default (option)
  "Return a display string for an nix OPTION default."
  (let ((default (nix-options-option-default option)))
    (if (null default)
        (propertize nix-options-empty-string 'face 'nix-options-not-given)
      (nix-options-fontify (json-encode default)))))

;; TODO: Replace `json-encode' with something like nix "builtins.fromJSON"
(defun nix-options-display-example (option)
  "Return a display string for a nix OPTION example."
  (let ((example (nix-options-option-example option)))
    (if (null example)
        (propertize nix-options-empty-string 'face 'nix-options-not-given)
      (nix-options-fontify
       (if (listp example)
           (if (assq '_type example)
               (let-alist example
                 (if (stringp .text)
                     .text
                   (json-encode .text)))
             (json-encode example))
         (json-encode example))))))

(defun nix-options-display-declarations (option)
  "Return a display string fo a nix OPTION declaration."
  (let ((declarations (nix-options-option-declarations option)))
    (string-join (mapcar (lambda (declaration)
                           (make-text-button declaration nil
                                             'follow-link t
                                             'action 'nix-options-decl-action
                                             'declaration declaration))
                         declarations)
                 "\n")))

;; Shamelessly stolen from `ansible-doc'.
(defun nix-options-fontify (text)
  "Add `font-lock-face' properties to nix mode.

Return a fontified copy of TEXT."
  ;; Graciously inspired by http://emacs.stackexchange.com/a/5408/227
  (if (not (fboundp 'nix-mode))
      text
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks
        (nix-mode)
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

(defun nix-options-decl-action (button)
  "Find file for declaration BUTTON."
  (interactive)
  (let ((declaration (button-get button 'declaration)))
    (find-file (nix-options-locate-declaration declaration))))

(defun nix-options-indent (string &optional length)
  "Indent STRING to LENGTH from line start."
  (replace-regexp-in-string "^" (make-string (or length nix-options-indent-default) 32) string))

(defun nix-options-option-string (option)
  "Return an option string from OPTION.

Used for show information about a nix option."
  (string-join (list
                (format "%s\n%s"
                        (propertize "NAME" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-option-name option)))
                (format "%s\n%s"
                        (propertize "DESCRIPTION" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-option-description option)))
                (format "%s\n%s"
                        (propertize "TYPE" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-option-type option)))
                (format "%s\n%s"
                        (propertize "DEFAULT" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-display-default option)))
                (format "%s\n%s"
                        (propertize "EXAMPLE" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-display-example option)))
                (format "%s\n%s"
                        (propertize "DECLARATIONS" 'face 'nix-options-value)
                        (nix-options-indent (nix-options-display-declarations option))))
               "\n"))

;;;###autoload
(defun nix-options-show (name)
  "Show information about nix option NAME."
  (interactive (list (completing-read "Option: "
                                      (hash-table-keys (nix-options))
                                      nil 'require-match nil 'nix-options-show-history
                                      (thing-at-point 'symbol))))
  (if-let (option (gethash name (nix-options)))
      (with-help-window (get-buffer-create nix-options-show-buffer-name)
        (with-current-buffer standard-output
          (let ((json-encoding-pretty-print t))
            (insert (nix-options-option-string option)))))
    (user-error "Not nix option %s found" name)))

;;;###autoload
(defun nix-options-load (&optional force from-homepage)
  "Populate `nix-options' variable from a json file.

If FORCE is non nil re-reads from file.  If FROM-HOMEPAGE is
non-nil will try to download the options json from NixOS
homepage."
  (interactive (list (if nix-options-loaded-p (y-or-n-p "Reload nix options? ") 'force)
                     (and current-prefix-arg (y-or-n-p "Use nix options from homepage? "))))
  (and force (setq nix-options-loaded-p nil))
  (nix-options from-homepage)
  (and force (called-interactively-p 'any) (message "Loaded NixOS options")))

(provide 'nix-options)
;;; nix-options.el ends here
