;;; nix-channel.el --- Nix generations integration -*- lexical-binding: t -*-

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

;; TODO:
;;
;; + Add `nix-channel-add'
;; + Add `nix-channel-update'
;; + Support channel generations `nix-env -p /nix/var/nix/profiles/per-user/root/channels --list-generations'.  See: https://github.com/matthiasbeyer/nixos-scripts/blob/master/nix-script-channel-list-generations.sh

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'tablist)
(require 'nix)
(require 'nix-package)

(defgroup nix-channel nil
  "Interface for NixOS genarations."
  :prefix "nix-channel-"
  :group 'nix-mode)

(defcustom nix-channel-executable "nix-channel"
  "Nix-channel executable."
  :type '(file :must-match t)
  :group 'nix-channel)

(cl-defstruct (nix-channel
               (:constructor nix-channel-new)
               (:type vector))
  "A struct holding the information of a Nix channel."
  name url)

(defun nix-channel-button-browse-url (button)
  "Open web browser on page pointed to by BUTTON."
  (browse-url (button-get button 'target)))

(define-button-type 'nix-channel-browse-url
  'action #'nix-channel-button-browse-url
  'help-echo "mouse-2, RET: goto homepage")

(defun nix-channel-entries ()
  "Return a list current available nix-env generations as `nix-channel' structs."
  (cl-loop for line in (nix-exec-lines nix-channel-executable "--list")
           collect (cl-multiple-value-bind (name url)
                       (split-string (string-trim line) "[[:space:]]+" 'omit-nulls)
                     (nix-channel-new :name name :url url))))

(defun nix-channel-list-entries ()
  "Return a entry `tabulated-list-entries' for nix generations."
  (cl-labels ((link-button (url) (make-text-button url nil 'type 'nix-package-browse-url 'target url)))
    (mapcar (lambda (chan)
              (list (nix-channel-name chan)
                    (vector (nix-channel-name chan)
                            (link-button (nix-channel-url chan)))))
            (nix-channel-entries))))

(defun nix-channel-do-add (url name)
  "Add a channel named name with URL url with NAME to the list of subscribed channels."
  (interactive (list (read-string "Channel url: ")
                     (read-string "Channel name: ")))
  (nix-exec nix-channel-executable "--add" url (unless (string-empty-p name) name)))

(defun nix-channel-do-update (&optional arg)
  "Install ARG entries."
  (interactive "P")
  (apply #'nix-exec nix-channel-executable "--update" (mapcar #'car (tablist-get-marked-items arg))))

(defun nix-channel-tablist-operations (operation &rest arguments)
  "Function for tablist OPERATION  and is called with ARGUMENTS.

See `tablist-operations-function' for more information."
  (cl-ecase operation
    (delete (cl-multiple-value-bind (ids) arguments
              (if (/= (length ids) 1)
                  (user-error "Each channel must be remove individually")
                (apply #'nix-exec nix-channel-executable "--remove" ids))))
    (find-entry (cl-multiple-value-bind (id) arguments
                  (let ((filename (format "/nix/var/nix/profiles/per-user/%s/channels/%s" (nix-login-name default-directory) id)))
                    (find-file (nix-file-relative filename default-directory)))))
    (supported-operations '(delete find-entry))))

(defvar nix-channel-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'tablist-find-entry)
    (define-key map "f" 'tablist-find-entry)
    (define-key map "D" 'tablist-do-delete)
    (define-key map "+" 'nix-channel-do-add)
    (define-key map "R" 'nix-channel-do-update)
    map)
  "Local keymap for `nix-channel-list-mode' buffers.")

(define-derived-mode nix-channel-list-mode tabulated-list-mode "nix-channels"
  "List available nix packages.

\\{nix-channel-list-mode-map}"
  (setq tabulated-list-format [("name" 30 t :read-only t)
                               ("url" 60 t :read-only t)]
        tabulated-list-padding 2
        tabulated-list-entries 'nix-channel-list-entries
        tablist-operations-function 'nix-channel-tablist-operations)
  (tabulated-list-init-header))

(add-hook 'nix-channel-list-mode-hook #'tablist-minor-mode)

;;;###autoload
(defun nix-channel-list ()
  "Show a list of available nix-packages."
  (interactive)
  (with-current-buffer (get-buffer-create
                        (if (file-remote-p default-directory)
                            (format "*nix-channels: %s*" (nix-file-relative ""))
                          "*nix-channels*"))
    (nix-channel-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(provide 'nix-channel)
;;; nix-channel.el ends here
