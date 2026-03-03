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

;; ----------------------------------------------------------------
;; GC (起動時 256MB, 起動後 24MB)
;; ----------------------------------------------------------------
(setq gc-cons-threshold (* 256 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

;; ----------------------------------------------------------------
;; 警告レベル
;; ----------------------------------------------------------------
(setq warning-minimum-level :error)

;; ----------------------------------------------------------------
;; custom-file
;; ----------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; ----------------------------------------------------------------
;; package.el 初期化 (emacs 27+)
;; ----------------------------------------------------------------
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; ----------------------------------------------------------------
;; use-package のロード
;; ----------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------------------------------------------------------
;; load-path
;; ----------------------------------------------------------------
(let ((dir (locate-user-emacs-file "lisp")))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;; ----------------------------------------------------------------
;; init-loader
;; ----------------------------------------------------------------
(use-package init-loader
  :config
  (setq init-loader-byte-compile t)
  (init-loader-load))

;;; init.el ends here
