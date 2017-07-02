;;; nixos-option.el --- NixOS options                 -*- lexical-binding: t -*-

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

;; See: <URL:https://nixos.org/nixos/options.html>.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist))

(require 'nix)
(require 'json)

(defgroup nix-option nil
  "Interface for NixOS options."
  :prefix "nixos-option-"
  :group 'languages)

(defcustom nixos-option-file nil
  "Nix options file."
  :type '(file :must-match t)
  :group 'nix-option)

(defcustom nixos-option-from-homepage-p nil
  "Whether to use nix-options file from homepage."
  :type 'booleanp
  :group 'nix-option)

(defvar nixos-option-indent-default 4)
(defvar nixos-option-show-history nil)
(defvar nixos-option-show-buffer-name "*NixOS option*")
(defvar nixos-option-process-environment '("LC_ALL=C" "NIXPKGS_ALLOW_UNFREE=1")
  "Prepended to `process-environment' while running nix-build.")
(defvar nixos-option-local-file (locate-user-emacs-file "nix-options.json")
  "Path where will be downloaded the nix options file from the NixOS homepage.")
(defvar nixos-option-options (make-hash-table :test 'equal)
  "Hash-table holding the information of NixOS options.")
(defvar nixos-option-loaded-p nil
  "Whether `nixos-option-options' hash-table is already loaded.")

(cl-defstruct (nixos-option
               (:constructor nil)
               (:copier nil)
               (:constructor nixos-option-option-new)
               (:type vector))
  "A structure holding all the information of a NixOS option."
  name declarations default description example readOnly type)

(defconst nixos-option-json-url "https://nixos.org/nixos/options.json.gz"
  "Url for nix options from the nixos homepage.")

(defun nixos-option-collect (json-file)
  "Collect nix candidates from the JSON-FILE."
  (seq-do (lambda (item)
            (let-alist (cdr item)
              (puthash (nix-as-string (car item))
                       (nixos-option-option-new
                        :name (nix-as-string (car item))
                        :default .default
                        :declarations .declarations
                        :description (string-trim .description)
                        :example .example
                        :readOnly .readOnly
                        :type .type)
                       nixos-option-options)))
          json-file))

;; TODO: Replace `json-encode' with something like nix "builtins.fromJSON"
(defun nixos-option-json-encode (object &optional pretty)
  "Return a JSON representation of OBJECT as a string, prettified if PRETTY is non-nil."
  (let ((json-encoding-pretty-print pretty))
    (json-encode object)))

(defun nixos-option-generate-json-options ()
  "Generate json options."
  (let* ((process-environment (append nixos-option-process-environment process-environment))
         (directory (nix-exec-string "nix-build" "-Q" "--no-out-link" "<nixpkgs/nixos/release.nix>" "-A" "options")))
    (expand-file-name "share/doc/nixos/options.json" directory)))

(defun nixos-option-http-fetch ()
  "Fetch nix options from NixOS homepage."
  (or (file-readable-p nixos-option-local-file) (url-copy-file nixos-option-json-url nixos-option-local-file))
  nixos-option-local-file)

(defun nixos-option-file (&optional from-homepage)
  "Get nixos-option-file or generate one.

If FROM-HOMEPAGE is non-nil, will download the json options from
the official website."
  (unless (and nixos-option-file (file-readable-p nixos-option-file))
    (message "Generating NixOS options file... ")
    (let ((file (if from-homepage
                    (nixos-option-http-fetch)
                  (condition-case err
                      (nixos-option-generate-json-options)
                    (error (if (y-or-n-p (format "Building nix options failed: %S.  Do you to use nix options from NixOS homepage? " (error-message-string err)))
                               (nixos-option-http-fetch)
                             (signal (car err) (cdr err))))))))
      (if (yes-or-no-p "Save the `nixos-option-file' for future sessions? ")
          (customize-save-variable 'nixos-option-file file)
        (customize-set-variable 'nixos-option-file file))))
  nixos-option-file)

(defun nixos-option-options (&optional force from-homepage)
  "Return the `nixos-option-options' hash-table, populate it if necessary.

If FORCE is non-nil will reload `nixos-option-options'.  If FROM-HOMEPAGE
is non-nil will download options file from nixos.org."
  (when (or force (not nixos-option-loaded-p))
    (message "loading NixOS options...")
    (nixos-option-collect (json-read-file (nixos-option-file (or from-homepage nixos-option-from-homepage-p))))
    (setq nixos-option-loaded-p t)
    (message "Loaded NixOS options"))
  nixos-option-options)

(defun nixos-option-locate-declaration (file-name)
  "Locate a nix declaration for nix FILE-NAME."
  (nix-exec-string "nix-instantiate" "--find-file" (concat "nixpkgs/" file-name)))

(defun nixos-option-display-default (option &optional pretty)
  "Return a string for an nix OPTION default, prettified if PRETTY is non-nil."
  (if-let (default (nixos-option-default option))
      (nix-fontify-text (nixos-option-json-encode default pretty) 'nix-mode)
    (propertize "Not specified" 'face 'nix-not-given)))

(defun nixos-option-display-example (option &optional pretty)
  "Return a string for a nix OPTION example, prettified if PRETTY is non-nil."
  (if-let (example (nixos-option-example option))
      (nix-fontify-text (if (and (listp example) (stringp (cdr (assq 'text example))))
                            (cdr (assq 'text example)) ; XXX: it is a literal example.
                          (nixos-option-json-encode example pretty))
                        'nix-mode)
    (propertize "Not specified" 'face 'nix-not-given)))

(defun nixos-option-display-declarations (option)
  "Return a display string of a nix OPTION declaration."
  (string-join (mapcar (lambda (declaration)
                         (make-text-button declaration nil
                                           'follow-link t
                                           'action 'nixos-option-decl-action
                                           'declaration declaration
                                           'help-echo "mouse-2, RET: goto declaration"))
                       (nixos-option-declarations option))
               "\n"))

(defun nixos-option-decl-action (button)
  "Find file for declaration BUTTON."
  (find-file-existing (nixos-option-locate-declaration (button-get button 'declaration))))

(defun nixos-option-indent (string &optional length)
  "Indent STRING to LENGTH from line start."
  (replace-regexp-in-string "^" (make-string (or length nixos-option-indent-default) 32) string))

;; TODO: replace <literal> and <option> tags.
(defun nixos-option-tostring (option)
  "Return an option string from OPTION.

Used for show information about a nix option."
  (nix-format-properties
   :name (propertize (nixos-option-name option) 'face 'nix-value)
   :description (nixos-option-description option)
   :type (nixos-option-type option)
   :default (nixos-option-display-default option 'pretty)
   :example (nixos-option-display-example option 'pretty)
   :declarations (nixos-option-display-declarations option)))

;;;###autoload
(defun nixos-option-show (name)
  "Show information about nix option NAME."
  (interactive (list (completing-read "Option: " (nixos-option-options) nil 'require-match nil 'nixos-option-show-history (thing-at-point 'symbol 'no-properties))))
  (if-let (option (gethash name (nixos-option-options)))
      (with-help-window (get-buffer-create nixos-option-show-buffer-name)
        (with-current-buffer standard-output
          (insert (nixos-option-tostring option))))
    (user-error "NixOS option %s not found" name)))

;;;###autoload
(defun nixos-option-load (&optional force from-homepage)
  "Populate `nixos-option-options' variable from a json file.

If FORCE is non-nil will reload `nixos-option-options' hash-table from
`nixos-option-file'.  If FROM-HOMEPAGE is non-nil will try to
download the options json from NixOS homepage."
  (interactive (list (if nixos-option-loaded-p (y-or-n-p "NixOS options is already loaded.  Reload it? ") 'force)
                     (and current-prefix-arg (y-or-n-p "Use NixOS options from homepage? "))))
  (nixos-option-options force from-homepage))

(provide 'nixos-option)
;;; nixos-option.el ends here
