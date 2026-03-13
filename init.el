;;; init.el --- My init entrypoint -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026  TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/12
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init entrypoint.

;;; Code:

;; ----------------------------------------------------------------
;; custom-file
;; ----------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; ----------------------------------------------------------------
;; use-package のロード
;; ----------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ----------------------------------------------------------------
;; init-loader
;; ----------------------------------------------------------------
(use-package init-loader
  :config
  (setq init-loader-byte-compile t)
  (init-loader-load))

(provide 'init)
;;; init.el ends here
