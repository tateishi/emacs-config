;;; 40-gt.el --- translation  -*- lexical-binding: t -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/05/28
;; Package-Requires: ((emacs "30.2"))

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

;; translation

;;; Code:

(eval-when-compile
  (require 'use-package))

;; ----------------------------------------------------------------
;; gt
;; ----------------------------------------------------------------
(use-package gt
  :custom
  (gt-langs '(ja en))

  :config
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :text 'paragraph)
         :engines (list (gt-google-engine))
         :render (gt-buffer-render)))

  )

(provide '40-gt)

;;; 40-gt.el ends here
