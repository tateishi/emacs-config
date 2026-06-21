;;; xdg-path.el --- xdg-path  -*- lexical-binding: t -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/06/21
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

;; Setting for XDG path

;;; Code:

(eval-when-compile
  (require 'use-package))

;; ----------------------------------------------------------------
;; XDG directories
;; ----------------------------------------------------------------
(require 'xdg)

(defconst my-xdg-cache (expand-file-name "emacs" (xdg-cache-home)))
(unless (file-directory-p my-xdg-cache) (make-directory my-xdg-cache))

(defconst my-xdg-config (expand-file-name "emacs" (xdg-config-home)))
(unless (file-directory-p my-xdg-config) (make-directory my-xdg-config))

(defconst my-xdg-data (expand-file-name "emacs" (xdg-data-home)))
(unless (file-directory-p my-xdg-data) (make-directory my-xdg-data))

;; provide
;; ----------------------------------------------------------------
(provide 'xdg-path)

;;; xdg-path.el ends here
