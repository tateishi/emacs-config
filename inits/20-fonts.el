;;; 20-fonts.el --- My init for font setting  -*- lexical-binding: t -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

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

;; My init for font setting.

;;; Code:


(defun my/create-fontset (font name)
  (message "%s:%s" font name)
  (condition-case err
      (create-fontset-from-ascii-font font nil name)
    (error
     (message "%s - %s" font (error-message-string err))
     nil)))

(defun my/create-fontset-from-list (font-list name)
  (message "%s:%s" font-list name)
  (if font-list
      (let ((res (my/create-fontset (car font-list) name)))
        (if res
            res
          (my/create-fontset-from-list (cdr font-list) name)))))

(defun my/select-favorite-font (fonts)
  (when (my/create-fontset-from-list fonts "font")
    (set-fontset-font "fontset-font"
                      'unicode "font" nil 'append)
    (add-to-list 'default-frame-alist '(font . "fontset-font"))))

(defvar font-list '(
                    "HackGen Console NF-14"
                    "HackGen Console-14"
                    "HackGenNerd Console-12"
                    "Myrica M-12"
                    "Noto Sans Mono CJK JP Regular-11"
                    "Noto Sans Mono CJK JP Regular-12"
                    "Ricty Diminished-12"
                    "Ricty Diminished-14"
                    "Myrica M-14"
                    "ＭＳ ゴシック-14"
                    ))

(use-package fonts
  :no-require t
  :if window-system
  :init
  (my/select-favorite-font font-list))

(provide '20-fonts)

;;; 20-fonts.el ends here
