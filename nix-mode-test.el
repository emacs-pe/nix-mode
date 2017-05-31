;;; nix-mode-test.el --- nix-mode: Unit test suite    -*- lexical-binding: t -*-

;; Copyright (C) 2017 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

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

;;; Code:
(declare-function undercover "undercover")

(and (require 'undercover nil 'no-error) (undercover "nix-mode.el"))

(require 'nix-mode)
(require 'nix)

(ert-deftest nix-mode-test-prettify ()
  "Test prettify regexp."
  (cl-loop for (file-name . result)
           in '(("/gnu/store/aiywpm2w299pk1ps96a8d8qwnwkzfr2g-foo-0.1" . "/gnu/store/…-foo-0.1")
                ("/nix/store/inb6pfvfm2vqpn9wlyrivj3iyx7k2pv6-foo-0.1" . "/nix/store/…-foo-0.1")
                ("https://hydra.gnu.org/nar/hrr424q661d9wdpkr48gyk5a9w8nrlcr-foo-0.1" . "https://hydra.gnu.org/nar/…-foo-0.1")
                ("https://hydra.gnu.org/log/fjbx25bap58k3mywzpmc8w9fjdydxqv8-foo-0.1" . "https://hydra.gnu.org/log/…-foo-0.1")
                ("https://bayfront.guixsd.org/nar/gzip/m4ccn9nzlsbvlj36w45555pq98spy007-foo-0.1" . "https://bayfront.guixsd.org/nar/gzip/…-foo-0.1"))
           do (should (string-equal (nix-prettify file-name) result))))

(provide 'nix-mode-test)
;;; nix-mode-test.el ends here
