;;; flycheck-nix.el --- Flycheck: nix support         -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Keywords: convenience, tools, languages
;; Version: 0.1
;; Package-Requires: ((emacs "24") (flycheck "0.22"))

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
;; This Flycheck extension provides a nix expressions syntax checker.

;;; Code:
(require 'flycheck)

(flycheck-define-checker nix
  "Nix checker using nix-instantiate."
  :command ("nix-instantiate" "--parse" source-inplace)
  :error-patterns
  ((error line-start
          "error: " (message) " at " (file-name) ":" line ":" column
          line-end))
  :modes nix-mode)

;;;###autoload
(defun flycheck-nix-setup ()
  "Setup Flycheck for nix.

Add `nix' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'nix))

(provide 'flycheck-nix)
;;; flycheck-nix.el ends here
