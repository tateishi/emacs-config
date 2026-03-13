;;; 10-user-local.el --- user local setting.  -*- lexical-binding: t -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/03/05
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

;; user local settings.

;;; Code:

(let* ((xdg (expand-file-name "user-local.el" (or (bound-and-true-p user-emacs-directory)
                                                  (locate-user-emacs-file ""))))
       (legacy (expand-file-name "user-local.el" "~/.emacs.d")))
  (cond
   ((file-readable-p xdg) (load xdg nil 'nomessage))
   ((file-readable-p legacy) (load legacy nil 'nomessage))))

(provide '10-user-local)

;;; 10-user-local.el ends here
