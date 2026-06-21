;;; 40-builtin.el --- emacs builtin functions  -*- lexical-binding: t -*-

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

;; Setting for Emacs buildin functions.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; ----------------------------------------------------------------
;; XDG directories
;; ----------------------------------------------------------------
(defconst my-xdg-cache
  (cond
   ((memq system-type '(gnu/linux darwin)) "~/.cache/emacs")
   ((eq system-type 'windows-nt) (expand-file-name "emacs/" (getenv "LOCALAPPDATA")))
   (t "~/.cache/emacs")))

(unless (file-directory-p my-xdg-cache) (make-directory my-xdg-cache))

;; ----------------------------------------------------------------
;; general setting
;; ----------------------------------------------------------------
(setopt indent-tabs-mode nil
        initial-buffer-choice t
        require-final-newline t
        vc-follow-symlinks t
        tab-always-indent 'complete
        use-short-answers t
        kill-do-not-save-duplicates t
        bookmark-file (expand-file-name "bookmarks" my-xdg-cache)
        recentf-max-saved-items 300
        recentf-auto-cleanup 'never
        recentf-save-file (expand-file-name "recentf" my-xdg-cache)
        save-interprogram-paste-before-kill t
        savehist-file (expand-file-name "savehist" my-xdg-cache)
        tramp-persistency-file (expand-file-name "tramp" my-xdg-cache)
        show-paren-delay 0.0
        show-paren-style 'parenthesis
        x-underline-at-descent-line t
        calendar-week-start-day 1
        calendar-latitude 35.18
        calendar-longitude 136.90
        calendar-location-name "名古屋市, 愛知県")

;; ----------------------------------------------------------------
;; global minor-modes
;; ----------------------------------------------------------------
(column-number-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(repeat-mode 1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(size-indication-mode 1)
(tab-bar-mode 1)
(tab-bar-history-mode 1)
(tool-bar-mode -1)
(which-key-mode t)
(winner-mode 1)

;; ----------------------------------------------------------------
;; global key map
;; ----------------------------------------------------------------
;;(keymap-global-set "C-h" #'backward-delete-char-untabify)
(keymap-global-set "C-z" #'scroll-down-command)
(keymap-global-set "C-x t b" #'tab-bar-history-back)
(keymap-global-set "C-x t f" #'tab-bar-history-forward)

;; repeat map for tab-bar-history-mode
(defvar my-tab-bar-history-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'tab-bar-history-back)
    (define-key map (kbd "f") #'tab-bar-history-forward)
    map)
  "Repeat map for tab bar history navigation.")

(put 'tab-bar-history-back 'repeat-map 'my-tab-bar-history-repeat-map)
(put 'tab-bar-history-forward 'repeat-map 'my-tab-bar-history-repeat-map)

;; ----------------------------------------------------------------
;; hooks
;; ----------------------------------------------------------------
(defun my-disable-trailing-whitespace ()
  "Disable `show-trailing-whitespace' in current buffer."
  (setq show-trailing-whitespace nil))

(defun my-enable-trailing-whitespace ()
  "Enable `show-trailing-whitespace' in current buffer."
  (setq show-trailing-whitespace t))

(defun my-delete-trailing-whitespace-maybe ()
  "Delete trailing whitespaces unless the major mode is in the blocklist."
  (unless (or (derived-mode-p 'markdown-mode 'org-mode 'diff-mode)
              (bound-and-true-p visual-line-mode))
    (delete-trailing-whitespace)))

;; 末尾空白の削除 除外するmodeはプログラム内で対応
(add-hook 'before-save-hook #'my-delete-trailing-whitespace-maybe)

;; 行番号の表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; 末尾空白の可視化
(add-hook 'prog-mode-hook #'my-enable-trailing-whitespace)
(add-hook 'text-mode-hook #'my-enable-trailing-whitespace)

;; 末尾空白の可視化抑制
(add-hook 'diff-mode-hook #'my-disable-trailing-whitespace)
(add-hook 'markdown-mode-hook #'my-disable-trailing-whitespace)
(add-hook 'org-mode-hook #'my-disable-trailing-whitespace)

;; ----------------------------------------------------------------
;; auto save
;; ----------------------------------------------------------------
(setopt auto-save-list-file-prefix (expand-file-name "auto-save/.saves-" my-xdg-cache))

;; ----------------------------------------------------------------
;; backup
;; ----------------------------------------------------------------

(defvar my-backup-dir (expand-file-name "backup" my-xdg-cache)
  "バックアップディレクトリ.")

(unless (file-directory-p my-backup-dir)
  (make-directory my-backup-dir))

(setopt backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        backup-directory-alist `(("." . ,my-backup-dir)))

;; ----------------------------------------------------------------
;; 日本語設定 UTF-8
;; ----------------------------------------------------------------
(setopt system-time-locale "ja_JP.UTF-8")
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-coding-system-priority 'utf-8 'utf-16 'utf-7 'utf-8-with-signature  ;; encodingの優先順位
                            'iso-2022-jp 'euc-jp 'japanese-shift-jis
                            'latin-1)
(set-charset-priority 'unicode 'japanese-jisx0208 'latin-iso8859-1)  ;; 文字集合の優先順位


;; ----------------------------------------------------------------
;; CJK ambiguous width chars are narrow
;; ----------------------------------------------------------------
;; t -> wide, nil -> narrow
(when (boundp 'cjk-ambiguous-chars-are-wide)
  (setopt cjk-ambiguous-chars-are-wide nil))

;; ----------------------------------------------------------------
;; for microsoft windows
;; ----------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (setq-default default-process-coding-system '(utf-8-dos . cp932)))

;; ----------------------------------------------------------------
;; recentf
;; ----------------------------------------------------------------
(require 'recentf)
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude "\\.cache/")
  (add-to-list 'recentf-exclude "\\.git/"))

;; ----------------------------------------------------------------
;; dabbrev
;; ----------------------------------------------------------------
(require 'dabbrev)
(setopt dabbrev-case-fold-search t)
(setopt dabbrev-case-replace nil)

;; ----------------------------------------------------------------
;; eshell
;; ----------------------------------------------------------------

(setopt  eshell-directory-name (expand-file-name "eshell" my-xdg-cache))

;; provide
;; ----------------------------------------------------------------
(provide '40-builtin)

;;; 40-builtin.el ends here
