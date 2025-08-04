;;;; init.el --- My init entrypoint -*- lexical-binding: t -*-


;; Copyright (C) 2021-2025  TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/12

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

(setq gc-cons-threshold (* 1024 1024 1024))
(defvar warning-minimum-level :emergency)

(defvar config-dir (file-name-directory load-file-name)
  "The root dir of the Emacs config.")

(setq custom-file (expand-file-name "custom.el" config-dir))
(if (file-exists-p custom-file) (load custom-file))


(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(let ((package 'use-package))
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(use-package init-loader
  :ensure t
  :functions init-locader-load
  :init (setq init-loader-byte-compile t)
  :config (init-loader-load))

;;; init.el ends here
