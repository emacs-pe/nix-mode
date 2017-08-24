;;; nixops.el --- Nix generations integration -*- lexical-binding: t -*-

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

(defgroup nixops nil
  "Interface for NixOS genarations."
  :prefix "nixops-"
  :group 'nix)

(defcustom nixops-executable "nixops"
  "Path to nixops executable."
  :type '(file :must-match t)
  :group 'nixops)

(cl-defstruct (nixops (:constructor nixops-new))
  "A struct holding the information of a Nix generation"
  uuid resource-id deployment machine status type ip-address)

(defun nixops-entries ()
  "Return a list current available nix-env generations as `nixops' structs."
  (cl-loop for line in (nix-exec-lines nixops-executable "info" "--all" "--plain")
           collect (cl-multiple-value-bind (uuid deployment machine status type resource-id ip-address)
                       (split-string line "\t")
                     (nixops-new :uuid uuid :resource-id resource-id :deployment deployment :machine machine :status status :type type :ip-address ip-address))))

(defun nixops-list-entries ()
  "Return a entry `tabulated-list-entries' for nix generations."
  (mapcar (lambda (d)
            (list (nixops-uuid d)
                  (vector (nixops-resource-id d)
                          (nixops-deployment d)
                          (nixops-machine d)
                          (nixops-status d)
                          (nixops-type d)
                          (nixops-ip-address d))))
          (nixops-entries)))

(defun nixops-find (id)
  "Find nix generation by ID."
  (let ((system-generation (format "/nix/var/nix/profiles/default-%s-link" id)))
    (if (file-exists-p (nix-file-relative system-generation))
        system-generation
      (format "/nix/var/nix/profiles/per-user/%s/profile-%s-link" (nix-login-name) id))))

(defun nixops-tablist-operations (operation &rest arguments)
  "Function for tablist OPERATION  and is called with ARGUMENTS.

See `tablist-operations-function' for more information."
  (cl-ecase operation
    (delete (cl-multiple-value-bind (ids) arguments
              (apply #'nix-exec nixops-executable "delete" "-d" ids)))
    (find-entry (cl-multiple-value-bind (id) arguments
                  (nix-find-file-relative (nixops-find id))))
    (supported-operations '(delete find-entry))))

(defun nixops-tramp-file-name (machine deployment)
  "Return a for MACHINE at DEPLOYMENT."
  ;; TODO: handle tramp hops
  (nix-make-tramp-file-name "nixops" machine nil deployment nil ""))

(defun nixops-ssh (&optional pos)
  "Open a ssh from POS."
  (interactive)
  (if-let (entry (tabulated-list-get-entry pos))
      (cl-multiple-value-bind (_resource deployment machine _status _type _ip) (append entry nil)
        (find-file (nixops-tramp-file-name machine deployment)))
    (user-error "Not nixops declaration at point")))

(defvar nixops-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'tablist-find-entry)
    (define-key map "f" 'tablist-find-entry)
    (define-key map "D" 'tablist-do-delete)
    map)
  "Local keymap for `nixops-list-mode' buffers.")

(define-derived-mode nixops-list-mode tabulated-list-mode "nixops"
  "List available nix packages.

\\{nixops-list-mode-map}"
  (setq tabulated-list-format [("resource"   36 t :read-only t)
                               ("deployment" 18 t :read-only t)
                               ("machine"    12 t :read-only t)
                               ("status"     12 t :read-only t)
                               ("type"       12 t :read-only t)
                               ("ip"         15 t :read-only t)]
        tabulated-list-padding 2
        tabulated-list-entries 'nixops-list-entries
        tablist-operations-function 'nixops-tablist-operations)
  (tabulated-list-init-header))

(add-hook 'nixops-list-mode-hook #'tablist-minor-mode)

;;;###autoload
(defun nixops-list ()
  "Show a list of available nix-packages."
  (interactive)
  (with-current-buffer (get-buffer-create (nix-tramp-buffer-name "nixops"))
    (nixops-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(provide 'nixops)
;;; nixops.el ends here
