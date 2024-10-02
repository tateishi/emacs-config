;;; my-functions.el  -*- lexical-binding: t; -*-


;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/09
;; Version: 0.0.1

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defun my/set-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(defun assoc-value  (key addr) (cdr (assoc key attr)))

(defun get-name     (attr) (assoc-value 'name attr))
(defun get-geometry (attr) (assoc-value 'geometry attr))
(defun get-workarea (attr) (assoc-value 'workarea attr))
(defun get-mm-size  (attr) (assoc-value 'mm-size attr))

(defun get-pixels-x  (attr) (caddr  (get-geometry attr)))
(defun get-pixels-y  (attr) (cadddr (get-geometry attr)))
(defun get-mm-size-x (attr) (car    (get-mm-size attr)))
(defun get-mm-size-y (attr) (cadr   (get-mm-size attr)))

(defun dpi-x (attr)
  (let ((px (get-pixels-x attr))
        (x (get-mm-size-x attr)))
    (/ px (/ x 25.4))))

(defun dpi-y (attr)
  (let ((py (get-pixels-y attr))
        (y (get-mm-size-y attr)))
    (/ py (/ y 25.4))))

(defun get-dpy ()
  (let ((attr (car (display-monitor-attributes-list))))
    (truncate (dpi-x attr))))

(defun date-header ()
  (format-time-string "# ================ %Y/%m/%d ================\n"))

(defun insert-date-header ()
  (interactive)
  (insert (date-header)))

(setq account-header-format "
#   %s
# ------------\n\n")

(defun insert-account-header (acc)
  (interactive "MAccount:")
  (insert (format account-header-format acc)))

(defun get-commit-message ()
  (format "%s 作業分" (format-time-string "%Y/%m/%d")))

(defun insert-commit-message ()
  (interactive)
  (insert (get-commit-message)))

(provide 'my-functions)
