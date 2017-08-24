;;; nix-package.el --- Nix package integration        -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (let-alist "1.0.1") (tablist "0.70"))

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

;; See: https://nixos.org/nixos/packages.html

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'let-alist)
  (defvar url-http-end-of-headers))

(require 'json)
(require 'tablist)
(require 'nix)

(defgroup nix-package nil
  "Interface for NixOS options."
  :prefix "nix-package-"
  :group 'languages)

(defcustom nix-package-nix-env-executable "nix-env"
  "Nix-env executable."
  :type '(file :must-match t)
  :group 'nix-package)

(defcustom nix-package-from-homepage-p nil
  "Whether to use nix-package file from homepage.

This option is provided on a best-effort basis, so not all the
features are available using this method.

See: `https://nixos.org/nixos/packages.html'."
  :type 'booleanp
  :group 'nix-package)

(defvar nix-package-show-history nil)
(defvar nix-package-truncate-width 79)
(defconst nix-package-location-regexp (rx (group-n 1 (+ (not (any " \n")))) ":" (group-n 2 (+ num)))
  "Regexp for location of a nix expression.")

(defvar-local nix-package-packages nil)

(cl-defstruct (nix-package
               (:constructor nil)
               (:copier nil)
               (:constructor nix-package-new)
               (:type vector))
  "A structure holding all the information of a NixOS option."
  name description homepage position)

(defconst nix-package-json-url "https://nixos.org/nixpkgs/packages.json.gz"
  "Url for nix options from the nixos homepage.")

(defun nix-package-truncate-string (string)
  "Truncate STRING with `nix-package-truncate-width'."
  (replace-regexp-in-string "[\n]" "" (truncate-string-to-width string nix-package-truncate-width nil nil 'ellipsis)))

(defun nix-package-collect (json-object)
  "Collect nix candidates from the JSON-OBJECT."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item json-object)
      (let-alist item
        (puthash (nix-as-string (car item))
                 (nix-package-new :name .name
                                  :homepage (or .meta.homepage .meta.downloadPage)
                                  :position .meta.position
                                  :description .meta.description)
                 table)))
    table))

(defun nix-package-generate-json (&rest _args)
  "Generate json from available packages with ARGS.

When FORCE is non-nil will recreate if already exists."
  (with-temp-buffer
    (nix-exec-insert nix-package-nix-env-executable "--query" "--available" "--json")
    (goto-char (point-min))
    (json-read)))

(defun nix-package-http-fetch ()
  "Fetch nix options from NixOS homepage and save it to FILE-NAME.

When FORCE is non-nil will recreate if already exists."
  (with-current-buffer (url-retrieve-synchronously nix-package-json-url)
    (goto-char (1+ url-http-end-of-headers))
    (cdr (assq 'packages (json-read-from-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))))))

(defun nix-package-data (&optional from-homepage)
  "Fetch data from nix packages.

If FROM-HOMEPAGE is non-nil, will download the json options from
the official website."
  (if from-homepage
      (nix-package-http-fetch)
    (condition-case err
        (nix-package-generate-json)
      (error
       (if (y-or-n-p (format "Building packages json failed: %S.  Do you to use nix options from NixOS homepage? " (error-message-string err)))
           (nix-package-http-fetch)
         (signal (car err) (cdr err)))))))

(cl-defun nix-package-packages ()
  "Populate `nix-package-packages' hash-table."
  (with-current-buffer (if (derived-mode-p 'nix-package-list-mode) (current-buffer) (get-buffer-create (nix-tramp-buffer-name "nix-packages")))
    (unless nix-package-packages
      (message "[%S] loading Nix packages..." (buffer-name))
      (setq-local nix-package-packages (nix-package-collect (nix-package-data nix-package-from-homepage-p))))
    nix-package-packages))

(defun nix-package-button-browse-url (button)
  "Open web browser on page pointed to by BUTTON."
  (browse-url (button-get button 'target)))

(defun nix-package-button-find-file (button)
  "Open web browser on page pointed to by BUTTON."
  (cl-multiple-value-bind (_match filename line)
      (nix-regexp-match nix-package-location-regexp (button-get button 'target))
    (if (file-exists-p (nix-file-relative filename))
        (with-current-buffer (find-file-noselect (nix-file-relative filename))
          (nix-goto-line (string-to-number line))
          (switch-to-buffer (current-buffer)))
      (error "%s does not exist" filename))))

(define-button-type 'nix-package-declaration
  'action #'nix-package-button-find-file
  'help-echo "mouse-2, RET: goto declaration")

(define-button-type 'nix-package-browse-url
  'action #'nix-package-button-browse-url
  'help-echo "mouse-2, RET: goto homepage")

(defun nix-package-entry-name (attr package)
  "Return the ATTR for PACKAGE."
  (if-let (declaration (nix-package-position package))
      (make-text-button (if (string-equal (nix-package-name package) "hook")
                            (format "%s (%s)" (nix-package-name package) attr)
                          (nix-package-name package))
                        nil
                        'type 'nix-package-declaration
                        'target declaration)
    (nix-package-name package)))

(defun nix-package-entry-homepage (_attr package)
  "Return an tabulated-list entry representation for ATTR and PACKAGE homepage."
  (if-let (homepage (nix-package-homepage package))
      (cl-labels ((link-button (url) (make-text-button url nil 'type 'nix-package-browse-url 'target url)))
        (cl-typecase homepage
          (vectorp (string-join (mapcar #'link-button homepage) ","))
          (stringp (link-button homepage))))
    ""))

(defun nix-package-entry-description (_attr package)
  "Return an tabulated-list entry representation for ATTR and PACKAGE description."
  (if-let (description (nix-package-description package)) (nix-package-truncate-string description) ""))

(defun nix-package-list-entries ()
  "Return a list of entries for `nix-package-list' tabulated mode."
  (cl-loop for attr being the hash-keys of (nix-package-packages)
           using (hash-value package)
           collect (list attr (vector (nix-package-entry-name attr package)
                                      (nix-package-entry-description attr package)
                                      (nix-package-entry-homepage attr package)))))

(defvar nix-package-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "S" 'nix-package-do-shell)
    (define-key map "i" 'nix-package-do-install)
    (define-key map "b" 'nix-package-browse)
    (define-key map (kbd "RET") 'nix-package-show)
    map)
  "Local keymap for `nix-package-list-mode' buffers.")

(define-derived-mode nix-package-list-mode tabulated-list-mode "nix-packages"
  "List available nix packages.

\\{nix-package-list-mode-map}"
  (setq tabulated-list-format [("name" 36 t :read-only t)
                               ("description" 80 nil :read-only t)
                               ("homepage" 120 nil :read-only t)]
        tabulated-list-padding 2
        tabulated-list-entries 'nix-package-list-entries)
  (tabulated-list-init-header))

(add-hook 'nix-package-list-mode-hook #'tablist-minor-mode)

(defun nix-package-read-attribute ()
  "Read nix package attribute."
  (let ((attribute (and (derived-mode-p 'nix-package-list-mode) (tabulated-list-get-id))))
    (list (or (and (not current-prefix-arg) attribute)
              (completing-read "Package attr: " (nix-package-packages) nil t attribute)))))

(defun nix-package-fetch-json (pkgname)
  "Return a `nix-package' struct for a nix PKGNAME."
  (cl-assert (not (string-blank-p pkgname)) nil "Package name must not be a empty string")
  (if-let (out (nix-exec-string nix-package-nix-env-executable "--query" "--available" "--json" "-A" pkgname))
      (cdr (assq (nix-as-symbol pkgname) (json-read-from-string out)))
    (user-error "Package %s not found" pkgname)))

(defun nix-package-do-shell (&optional arg)
  "Install ARG entries."
  (interactive "P")
  (cl-assert (tablist-get-marked-items arg) nil "You need to select a package(s) to install")
  (apply #'nix-shell "-p" (mapcar (lambda (item) (nix-attribute-to-package (car item))) (tablist-get-marked-items arg))))

(defun nix-package-do-install (&optional arg)
  "Install ARG entries."
  (interactive "P")
  (cl-assert (tablist-get-marked-items arg) nil "You need to select a package(s) to install")
  (apply #'nix-exec nix-package-nix-env-executable (cl-mapcan (lambda (item) (list "-iA" (car item))) (tablist-get-marked-items arg))))

;;;###autoload
(defun nix-package-install (attribute)
  "Install nix package ATTRIBUTE."
  (interactive (nix-package-read-attribute))
  (nix-exec nix-package-nix-env-executable "-iA" attribute))

;;;###autoload
(defun nix-package-browse (attribute)
  "Build an browse a package ATTRIBUTE."
  (interactive (nix-package-read-attribute))
  (cl-multiple-value-bind (repo package)
      (nix-attribute-spec attribute)
    (nix-find-file-relative (nix-exec-string "nix-build" (format "<%s>" repo) "--no-out-link" "-A" package))))

(defvar nix-shell-buffer-name)
(declare-function nix-shell "nix-shell")

;;;###autoload
(defun nix-package-shell (attribute)
  "Install nix package ATTRIBUTE."
  (interactive (nix-package-read-attribute))
  (let* ((package-name (nix-attribute-to-package attribute))
         (nix-shell-buffer-name (format "*nix-shell/%s*" package-name)))
    (nix-shell "-p" package-name)))

;;;###autoload
(defun nix-package-show (attribute)
  "Display information about nix package ATTRIBUTE."
  (interactive (nix-package-read-attribute))
  (let-alist (nix-package-fetch-json attribute)
    (cl-labels ((show-license (license) (cl-typecase license
                                          (stringp license)
                                          (listp (let-alist license
                                                   (let ((license-name (or .fullName .shortName .spdxId)))
                                                     (if .url (nix-link-button .url license-name) license-name))))))
                (show-homepage (url) (cl-typecase url
                                       (stringp (nix-link-button url))
                                       (vectorp (apply #'nix-join-lines (mapcar #'nix-link-button url))))))
      (nix-insert-format "*NixPkg*"
        :package .name
        :attribute (propertize attribute 'face 'nix-value)
        :homepage (show-homepage (or .meta.homepage .meta.downloadPage))
        :license (show-license .meta.license)
        :system .system
        :position (make-text-button (nix-prettify .meta.position) nil 'type 'nix-package-declaration 'target .meta.position)
        :description (or .meta.longDescription .meta.description)))))

;;;###autoload
(defun nix-package-list ()
  "Show a list of available nix-packages."
  (interactive)
  (with-current-buffer (get-buffer-create (nix-tramp-buffer-name "nix-packages"))
    (nix-package-list-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(provide 'nix-package)
;;; nix-package.el ends here
