;;; my-utils.el --- Utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/06/25
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

;; Utilities

;;; Code:

;;
;; コミット用のメッセージ
;;
(defun insert-commit-message ()
  "作業日を使った定型のメッセージを入力する."
  (interactive)
  (let ((message (format "%s 作業分" (format-time-string "%Y-%m-%d"))))
    (insert message)))

;;
;; OSC52 clipboard
;;
(defun osc52-copy-region (start end)
  "Copy the region from START to END to the system clipboard via OSC 52.

The text is encoded as UTF-8 to handle multibyte characters, Base64-encoded,
and sent to the terminal using the OSC 52 escape sequence.

Note:
- Requires a terminal that supports OSC 52 clipboard operations.
- When running over SSH or inside tmux/screen, clipboard forwarding may need
  to be explicitly enabled.
- Some terminals restrict or disable OSC 52 for security reasons."

  (interactive "r")
  (let ((text (encode-coding-string (buffer-substring-no-properties start end) 'utf-8)))
    (send-string-to-terminal
     (concat "\e]52;c;" (base64-encode-string text t) "\a"))))

(provide 'my-utils)
;;; my-utils.el ends here
