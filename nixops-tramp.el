;;; nixops-tramp.el --- TRAMP integration for nixops deployments -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: nixops, convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
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
;; `nixops-tramp.el' offers a TRAMP method for [nixops][] deployments.
;;
;; ## Usage
;;
;; Offers the TRAMP method `nixops` to access nixops deployments
;;
;;    C-x C-f /nixops:machine@deployment:/path/to/file
;;
;;    where
;;      machine       (required) is the name of the machine you want to use
;;      deployment    (required) is the name of the nixops deployment
;;
;; [nixops]: https://github.com/NixOS/nixops "NixOS cloud provisioning and deployment tool"

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'tramp)

(defgroup nixops-tramp nil
  "TRAMP integration for nixops deployments."
  :prefix "nixops-tramp-"
  :group 'applications)

(defcustom nixops-tramp-nixops-executable "nixops"
  "Path to nixops executable."
  :type 'string
  :group 'nixops-tramp)

(defcustom nixops-tramp-nixops-ssh-options nil
  "List of nixops ssh options."
  :type '(repeat string)
  :group 'nixops-tramp)

(defcustom nixops-tramp-nixops-scp-options nil
  "List of nixops scp options."
  :type '(repeat string)
  :group 'nixops-tramp)

;;;###autoload
(defconst nixops-tramp-method "nixops"
  "Method to connect nixops machines.")

;;;###autoload
(defconst nixops-tramp-completion-function-alist
  '((nixops-tramp-machines-and-deployments ""))
  "Default list of (FUNCTION FILE) pairs to be examined for nixops method.")

(defun nixops-tramp-machines-and-deployments (&optional _ignored)
  "Return a list of (machine deployment) tuples allowed to access.

TRAMP calls this function with a filename which is IGNORED."
  (cl-loop for line in (ignore-errors (process-lines nixops-tramp-nixops-executable "info" "--all" "--plain"))
           collect (cl-multiple-value-bind (_resource-id deployment machine _status _type _ip-address)
                       (split-string line "[\t]+" 'omit-nulls)
                     (list machine deployment))))

;;;###autoload
(defun nixops-tramp-add-method ()
  "Add nixops tramp method."
  (add-to-list 'tramp-methods
               `(,nixops-tramp-method
                 (tramp-login-program        ,nixops-tramp-nixops-executable)
                 (tramp-login-args           (("ssh") ,nixops-tramp-nixops-ssh-options
                                              ("-d") ("%h") ("%u") ("-p" "%p")
                                              ("%c") ("-e" "none")))
                 (tramp-remote-shell         "/bin/sh")
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-copy-program         ,nixops-tramp-nixops-executable)
                 (tramp-copy-args            (("scp") (,nixops-tramp-nixops-scp-options ("-d") ("%h") ("%u"))))
                 (tramp-copy-keep-date       t)
                 (tramp-copy-recursive       t))))

;;;###autoload
(eval-after-load 'tramp
  '(progn
     (nixops-tramp-add-method)
     (tramp-set-completion-function nixops-tramp-method nixops-tramp-completion-function-alist)))

(provide 'nixops-tramp)
;;; nixops-tramp.el ends here
