;;; nix-generation.el --- Nix generations integration -*- lexical-binding: t -*-

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

;; Nix generations integration

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'tablist)
(require 'nix)
(require 'nix-package)

(defgroup nix-generation nil
  "Interface for NixOS genarations."
  :prefix "nix-generation-"
  :group 'languages)

(cl-defstruct (nix-generation
               (:constructor nix-generation-new)
               (:type vector))
  "A struct holding the information of a Nix generation"
  number date current)

(defun nix-generation-entries ()
  "Return a list current available nix-env generations as `nix-generation' structs."
  (cl-loop for line in (nix-exec-lines nix-package-nix-env-executable "--list-generations")
           collect (cl-multiple-value-bind (number date current)
                       (split-string (string-trim line) "[[:space:]][[:space:]]+" 'omit-nulls)
                     (nix-generation-new :number number :date date :current (string-equal current "(current)")))))

(defun nix-generation-list-entries ()
  "Return a entry `tabulated-list-entries' for nix generations."
  (mapcar (lambda (gen)
            (list (nix-generation-number gen)
                  (vector (nix-generation-number gen)
                          (nix-generation-date gen)
                          (if (nix-generation-current gen) "●" ""))))
          (nix-generation-entries)))

(defun nix-generation-do-remove (&optional arg)
  "Install ARG entries."
  (interactive "P")
  (apply #'nix-exec nix-package-nix-env-executable
         "--delete-generations"
         (cl-loop for (attr . _entry) in (tablist-get-marked-items arg)
                  collect attr)))

(defun nix-generation-find (id)
  "Find nix generation by ID."
  (let ((system-generation (format "/nix/var/nix/profiles/default-%s-link" id)))
    (if (file-exists-p (nix-file-relative system-generation))
        system-generation
      (format "/nix/var/nix/profiles/per-user/%s/profile-%s-link" (nix-login-name) id))))

(defun nix-generation-tablist-operations (operation &rest arguments)
  "Function for tablist OPERATION  and is called with ARGUMENTS.

See `tablist-operations-function' for more information."
  (cl-ecase operation
    (delete (cl-multiple-value-bind (ids) arguments
              (apply #'nix-exec nix-package-nix-env-executable "--delete-generations" ids)))
    (find-entry (cl-multiple-value-bind (id) arguments
                  (nix-find-file-relative (nix-generation-find id))))
    (supported-operations '(delete find-entry))))

(defvar nix-generation-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'tablist-find-entry)
    (define-key map "f" 'tablist-find-entry)
    (define-key map "D" 'tablist-do-delete)
    map)
  "Local keymap for `nix-generation-list-mode' buffers.")

(define-derived-mode nix-generation-list-mode tabulated-list-mode "nix-generations"
  "List available nix packages.

\\{nix-generation-list-mode-map}"
  (setq tabulated-list-format [("number" 10 t :read-only t)
                               ("date" 30 t :read-only t)
                               ("current" 30 t :read-only t)]
        tabulated-list-padding 2
        tabulated-list-entries 'nix-generation-list-entries
        tablist-operations-function 'nix-generation-tablist-operations)
  (tabulated-list-init-header))

(add-hook 'nix-generation-list-mode-hook #'tablist-minor-mode)

;;;###autoload
(defun nix-generation-list ()
  "Show a list of available nix-packages."
  (interactive)
  (with-current-buffer (get-buffer-create (nix-tramp-buffer-name "nix-generations"))
    (nix-generation-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(provide 'nix-generation)
;;; nix-generation.el ends here
