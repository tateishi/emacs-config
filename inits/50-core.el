;;; 50-core.el --- My init core  -*- lexical-binding: t -*-

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

;; My init core.

;;; Code:

;; ----------------------------------------------------------------
;; general setting
;; ----------------------------------------------------------------
(use-package emacs
  :preface
  (defun my-disable-trailing-whitespace ()
    "Disable `show-trailing-whitespace' in current buffer."
    (setq show-trailing-whitespace nil))

  (defun my-enable-trailing-whitespace ()
    "Enable `show-trailing-whitespace' in current buffer."
    (setq show-trailing-whitespace t))

  (defun my-delete-trailing-whitespace-maybe ()
    "Delete trailing whitespaces unless the major mode is in the blocklist."
    (unless (or (derived-mode-p 'markdown-mode 'org-mode 'diff-mode 'magit-mode)
                (bound-and-true-p visual-line-mode))
      (delete-trailing-whitespace)))

  :custom
  (indent-tabs-mode nil)
  (initial-buffer-choice t)
  (require-final-newline t)
  (vc-follow-symlinks t)
  (tab-always-indent 'complete)
  (use-short-answers t)
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)
  (show-paren-delay 0.0)
  (show-paren-style 'parenthesis)

  :init
  (setq x-underline-at-descent-line t)

  :config
  (column-number-mode t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode t)
  (size-indication-mode t)
  (tab-bar-mode 1)
  (tool-bar-mode -1)

  :bind
  (("C-h" . backward-delete-char-untabify)
   ("C-z" . scroll-down-command)
   ("<C-return>" . other-window)
   ("<M-return>" . other-frame))

  :hook
  ;; 末尾空白の削除 除外するmodeはプログラム内で対応
  (before-save . my-delete-trailing-whitespace-maybe)
  ;; 行番号の表示
  (prog-mode . display-line-numbers-mode)
  ;; 末尾空白の可視化
  (prog-mode . my-enable-trailing-whitespace)
  (text-mode . my-enable-trailing-whitespace)
  ;; 末尾空白の可視化抑制
  (markdown-mode . my-disable-trailing-whitespace)
  (org-mode . my-disable-trailing-whitespace)
  (diff-mode . my-disable-trailing-whitespace)
  (git-mode . my-disable-trailing-whitespace))

;; ----------------------------------------------------------------
;; 日本語設定 UTF-8
;; ----------------------------------------------------------------
(use-package japanese
  :no-require t

  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  ;; encodingの優先順位
  (set-coding-system-priority
   'utf-8 'utf-16 'utf-7 'utf-8-with-signature
   'iso-2022-jp 'euc-jp 'japanese-shift-jis
   'latin-1)
  ;; 文字集合の優先順位
  (set-charset-priority
   'unicode 'japanese-jisx0208 'latin-iso8859-1)

  :custom
  (system-time-locale "ja_JP.UTF-8"))

;; ----------------------------------------------------------------
;; clip board
;; ----------------------------------------------------------------
(use-package clipetty
  :if (not (or (display-graphic-p) noninteractive))
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))

;; ----------------------------------------------------------------
;; keyfreq
;; ----------------------------------------------------------------
(use-package keyfreq
  :if (not noninteractive)
  :ensure t
  :init
  (make-directory (locate-user-emacs-file "keyfreq") t)
  (setq keyfreq-file (locate-user-emacs-file "keyfreq/keyfreq.el"))
  (setq keyfreq-file-lock (locate-user-emacs-file "keyfreq/keyfreq.lock"))
  (setq keyfreq-excluded-commands '(self-insert-command))

  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ----------------------------------------------------------------
;; Google mozc
;; ----------------------------------------------------------------
(use-package mozc
  :disabled t
  :ensure t
  :custom
  (default-input-method "japanese-mozc"))

;; ----------------------------------------------------------------
;; DDSKK
;; ----------------------------------------------------------------
(defun my/with-jisyo-dir (dir jisyo-list)
  "DIR を JISYO-LIST の各要素に付けて返す。

JISYO-LIST は
  (\"FILE\")
  または (\"FILE\" . CODING)
のどちらでも良い。"
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (mapcar (lambda (e)
              (pcase e
                ;; ("SKK-JISYO.xxx" . euc-japan) の形
                (`(,file . ,coding)
                 (cons (expand-file-name file dir) coding))
                ;; "SKK-JISYO.xxx" の形
                ((pred stringp)
                 (expand-file-name e dir))
                ;; それ以外はそのまま（想定外入力でも落ちにくく）
                (_ e)))
            jisyo-list)))

(put 'my/with-jisyo-dir 'lisp-indent-function 1)

(use-package ddskk
  :ensure t
  :init
  (make-directory (locate-user-emacs-file "ddskk") t)
  (make-directory (locate-user-emacs-file "skk") t)
  :custom
  (default-input-method "japanese-skk")
  (skk-user-directory (locate-user-emacs-file "ddskk"))
  (skk-large-jisyo (locate-user-emacs-file "skk/SKK-JISYO.L"))
  (skk-extra-jisyo-file-list (my/with-jisyo-dir (locate-user-emacs-file "skk")
                               '(("SKK-JISYO.jinmei" . euc-japan)
                                 ("SKK-JISYO.zipcode" . euc-japan))))
  (skk-sticky-key ";")
  :bind
  (("C-x C-j" . skk-mode))
  :config
  (setq skk-get-jisyo-directory (locate-user-emacs-file "skk")))

;; ----------------------------------------------------------------
;; CJK ambiguous width chars are narrow
;; ----------------------------------------------------------------
(use-package cjk-ambiguous
  :no-require t
  :if (boundp 'cjk-ambiguous-chars-are-wide)

  :preface
  (defun my-cjk-ambiguous-width ()
    (setopt cjk-ambiguous-chars-are-wide nil)
    (when (fboundp 'clear-font-cache)
      (clear-font-cache)))

  :hook
  (emacs-startup . my-cjk-ambiguous-width))

;; ----------------------------------------------------------------
;; for microsoft windows
;; ----------------------------------------------------------------
(use-package ms-windows
  :no-require t
  :if (eq system-type 'windows-nt)
  :config
  (setq-default default-process-coding-system '(utf-8-dos . cp932)))

;; ----------------------------------------------------------------
;; ORG mode
;; ----------------------------------------------------------------
;; helper commands/functions.
(defun my-show-org-buffer (file)
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (expand-file-name file org-directory))))

(defun my-show-org-notes-buffer ()
  (interactive)
  (my-show-org-buffer "notes.org"))

;; package definition
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c b" . my-show-org-notes-buffer))
  :custom
  (org-agenda-files (if (file-exists-p org-directory)
                        (directory-files-recursively org-directory "\\.org$")))
  (org-timestamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
  (org-capture-templates
   '(("n" "Note" entry (file+headline "~/org/notes.org" "Notes") "* %?\nEntered on %U\n %i\n %a")))
  (org-refile-targets '((org-agenda-files :maxlevel . 3))))

(use-package org-journal
  :ensure t
  :bind (("C-c j" . org-journal-new-entry))
  :custom
  (org-journal-dir "~/org/local/journal/")
  (org-journal-date-format "%Y-%m-%d (%A)"))

;; ----------------------------------------------------------------
;; repeat-mode
;; ----------------------------------------------------------------
(use-package emacs
  :if (>= emacs-major-version 28)
  :config
  (repeat-mode 1))

;; ----------------------------------------------------------------
;; backup
;; ----------------------------------------------------------------
(use-package emacs
  :config
  (let ((dir (locate-user-emacs-file "backup")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setopt backup-directory-alist
            `(("." . ,dir))))
  :custom
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

;; ----------------------------------------------------------------
;; undo tree
;; ----------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist (list (cons ".*" (locate-user-emacs-file "undo"))))
  :config
  (let ((dir (locate-user-emacs-file "undo")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setopt undo-tree-history-directory-alist
            `(("." . ,dir))))
  (global-undo-tree-mode))

;; ----------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------
(use-package avy
  :ensure t
  :bind
  ("C-c :" . avy-goto-char-timer))

;; ----------------------------------------------------------------
;; 補完パッケージ company
;; ----------------------------------------------------------------
(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (setq company-transformers '(company-sort-by-statistics
                               company-sort-by-occurrence
                               company-sort-by-backend-importance))
  (company-statistics-mode))

;; ----------------------------------------------------------------
;; vertico
;; ----------------------------------------------------------------
(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  :config
  (vertico-mode))

;; ----------------------------------------------------------------
;; consult
;; ----------------------------------------------------------------
(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x l" . consult-line)
   ("C-x m" . consult-grep)))

;; ----------------------------------------------------------------
;; orderless
;; ----------------------------------------------------------------
(use-package orderless
  :ensure t
  :after consult
  :preface
  (defun my-orderless-migemo-dispatcher (pattern _index _total)
    (when (and (featurep 'migemo)
               (string-match-p "\\`[[:ascii:]]+\\'" pattern))
      `(orderless-regexp . ,(migemo-get-pattern pattern))))

  (defun my-consult--migemo-regexp-compiler (input type ignore-case)
    (let ((migemo-regexp (mapcar #'migemo-get-pattern (consult--split-escaped input))))
      (cons (mapcar (lambda (reg) (consult--convert-regexp reg type)) migemo-regexp)
            (lambda (str) (consult--highlight-regexps migemo-regexp ignore-case str)))))

  :custom
  (completion-styles '(orderless basic partial-completion emacs22))
  (completion-category-overrides '((file (styles basic partial-completion)
                                         (display-sort-function . minibuffer-sort-alphabetically))))
  (orderless-style-dispatchers '(my-orderless-migemo-dispatcher))
  :config
  (setq consult--regexp-compiler #'my-consult--migemo-regexp-compiler))

;; ----------------------------------------------------------------
;; marginalia
;; ----------------------------------------------------------------
(use-package marginalia
  :ensure t
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-annotators
               '(command marginalia-annotate-command builtin))
  (marginalia-mode))

;; ----------------------------------------------------------------
;; google translate
;; ----------------------------------------------------------------
(use-package google-translate
  :ensure t
  :defines google-translate-translation-directions-alist
  :bind
  (("C-c t" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "ja")
                                                        ("ja" . "en")))
  )

;; ----------------------------------------------------------------
;; smartparens
;; ----------------------------------------------------------------
(use-package smartparens
  :ensure t
  :custom
  (electric-pair-mode nil)
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode))

;; ----------------------------------------------------------------
;; rainbow-delimiters
;; ----------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; ----------------------------------------------------------------
;; highlight-indent-guides
;; ----------------------------------------------------------------
(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-character ?|))

;; ----------------------------------------------------------------
;; whitespace-mode
;; ----------------------------------------------------------------
(use-package whitespace
  :ensure t
  :bind ("C-c W" . whitespace-cleanup)
  :custom
  (whitespace-style '(face trailing tabs spaces empty space-mark tab-mark))
  (whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])
                                 (tab-mark ?\t [?\u00BB ?\t]
                                           [?\\ ?\t])))
  (whitespace-space-regexp "\\(\u3000+\\)")
  (whitespace-global-modes '(not dired-mode tar-mode magit-mode))
  (global-whitespace-mode t)
  (whitespace-action '(auto-cleanup))
  :custom-face
  ;; (whitespace-trailing ((t (:background "Black" :foreground "DeepPink" :underline t))))
  ;; (whitespace-tab ((t (:background "Black" :foreground "LightSkyBlue" :underline t))))
  ;; (whitespace-space ((t (:background "Black" :foreground "GreenYellow" :weight bold))))
  ;; (whitespace-empty ((t (:background "Black"))))
  )

;; ----------------------------------------------------------------
;; Eglot (emacs client of language server)
;; ----------------------------------------------------------------
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp")))

;; ----------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode))

;; ----------------------------------------------------------------
;; cc mode
;; ----------------------------------------------------------------
(use-package cc-mode
  :ensure t
  :hook
  (c-mode-common . my-enable-trailing-whitespace))

(use-package google-c-style
  :ensure t
  :after cc-mode
  :hook
  (c-mode-common . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

;; ----------------------------------------------------------------
;; python mode
;; ----------------------------------------------------------------
(use-package python-mode
  :ensure t
  :hook
  (python-mode-hook . smartparens-mode))

;; ----------------------------------------------------------------
;; python-black
;; ----------------------------------------------------------------
(use-package python-black
  :ensure t
  :after python)

;; ----------------------------------------------------------------
;; py-isort
;; ----------------------------------------------------------------
(use-package py-isort
  :ensure t
  :after python)

;; ----------------------------------------------------------------
;; markdown-mode
;; ----------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

;; ----------------------------------------------------------------
;; jinja2-mode
;; ----------------------------------------------------------------
(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" "\\.jinja\\'" "\\.jinja2\\'"))

;; ----------------------------------------------------------------
;; ledger-mode / shiwake-mode
;; ----------------------------------------------------------------
(defun my-ledger-comment-column ()
  "Set comment column for ledger-mode."
  (setq-local comment-column 55))

(use-package shiwake-mode
  :vc (:url "https://github.com/tateishi/shiwake-mode/" :rev :newest)
  :commands (shiwake-mode)
  :custom
  (ledger-accounts-file "~/wks/ledger/accounts/accounts.dat")
  (ledger-payees-file "~/wks/ledger/accounts/payees.dat")
  (shiwake-tagnames-file "~/wks/ledger/accounts/tags.dat")
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-post-amount-alignment-at :decimal)
  (ledger-complete-in-steps t)
  (ledger-default-date-format ledger-iso-date-format)
  (ledger-reports
   '(("残高" "%(binary) -f %(ledger-file) --sort date,-amount reg %(account)")
     ("店別" "%(binary) -f %(ledger-file) --sort date,-amount reg @%(payee)")
     ("TAG別" "%(binary) -f %(ledger-file) --sort date,-amount reg %(account) and \%%(tagname)")
     ("META" "%(binary) -f %(ledger-file) reg %(account) --limit \"tag('item') == 'what'\"")
     ("bal" "%(binary)  -f %(ledger-file) bal")
     ("bal this month" "%(binary) -f %(ledger-file)  -p %(month) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :hook
  (ledger-mode . my-enable-trailing-whitespace)
  (ledger-mode . my-ledger-comment-column)
  :config
  (add-to-list 'ledger-report-format-specifiers '("tagname" . shiwake-report-tagname-format-specifier)))

;; ----------------------------------------------------------------
;; kinshu mode
;; ----------------------------------------------------------------
(use-package kinshu-mode
  :vc (:url "https://github.com/tateishi/kinshu-mode/" :rev :newest))

;; ----------------------------------------------------------------
;; web-mode
;; url: https://web-mode.org/
;; ----------------------------------------------------------------
(use-package web-mode
  :ensure t
  :commands (web-mode)
  :mode (("\\.html\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-engine-detection t)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0))

;; ----------------------------------------------------------------
;; typescript
;; ----------------------------------------------------------------
(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :custom
  (typescript-indent-level 2))

;; ----------------------------------------------------------------
;; json
;; ----------------------------------------------------------------
(use-package json-mode
  :ensure t
  :commands (json-mode)
  :custom
  (js-indent-level 2))

(use-package prettier-js
  :ensure t
  :hook
  (typescript-mode . prettier-js-mode)
  (tsx-mode . prettier-js-mode))

;; ----------------------------------------------------------------
;; kanata-mode
;; ----------------------------------------------------------------
(use-package kanata-kbd-mode
  :vc (:url "https://github.com/chmouel/kanata-kbd-mode/" :rev :newest))

;; ----------------------------------------------------------------
;; progmodes
;; ----------------------------------------------------------------
(defmacro expand-packages (&rest packages)
  "PACKAGES を順にuse-packageに適用する.

\(expand-package
  package-1
  package-2\)
=>
\(progn
  \(use-package package-1 :ensure t\)
  \(use-package package-2 :ensure t\)\)"
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (p) `(use-package ,p :ensure t)) packages)))

(expand-packages
  ansible
  cmake-mode
  csv-mode
  docker-compose-mode
  dockerfile-mode
  haskell-mode
  ledger-mode
  lua-mode
  meson-mode
  rust-mode
  yaml-mode)

;; ----------------------------------------------------------------
;; yasnippet
;; ----------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; ----------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------
(use-package magit
  :ensure t
  :bind
  (("C-x C-g" . magit-status)
   ("C-x g"   . magit-status)))

;; ----------------------------------------------------------------
;; migemo
;; ----------------------------------------------------------------
(setq my-migemo-path "/usr/share/cmigemo/utf-8")

(defun my-migemo-directory (path)
  (if (file-directory-p path)
      path
    (let ((migemo-path (file-name-directory (or (executable-find "cmigemo") ""))))
      (expand-file-name "dict/utf8" migemo-path))))

(defun my-migemo-dictionary (path)
  (if (file-directory-p path)
      (expand-file-name "migemo-dict" path)))

(when (executable-find "cmigemo")
  (use-package migemo
    :ensure t
    :functions migemo-init
    :config
    (setq migemo-directory (my-migemo-directory my-migemo-path))
    (setq migemo-dictionary (my-migemo-dictionary my-migemo-path))
    (setq migemo-user-dictionary (expand-file-name "user-dict" "~/.local/share/cmigemo/"))
    (setq migemo-options `("-q" "--emacs" "-s" ,migemo-user-dictionary))
    (migemo-init)))

;; ----------------------------------------------------------------
;; open junk file
;; ----------------------------------------------------------------
(use-package open-junk-file
  :ensure t
  :bind
  ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format "~/.texts/%Y/%m/%Y-%m-%d-%H%M%S."))

;; ----------------------------------------------------------------
;; modus themes
;; url: https://gitlab.com/protesilaos/modus-themes
;; url: https://protesilaos.com/emacs/modus-themes
;; ----------------------------------------------------------------
(use-package modus-themes
  :ensure t
  :init
  (modus-themes-select 'modus-vivendi)
  :custom
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  :config
  :bind ("<f5>" . modus-themes-toggle))

;; ----------------------------------------------------------------
;; calendar mode
;; ----------------------------------------------------------------
(use-package calendar
  :ensure t
  :custom
  (calendar-week-start-day 1)
  (calendar-latitude 35.18)
  (calendar-longitude 136.90)
  (calendar-location-name "名古屋市, 愛知県"))

;; ----------------------------------------------------------------
;; which-key
;; ----------------------------------------------------------------
(use-package which-key
  :ensure t
  :config (which-key-mode t))

;; ----------------------------------------------------------------
;; persp-mode
;; ----------------------------------------------------------------
(use-package persp-mode
  :ensure t
  :config (persp-mode t))

;; ----------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------
(use-package treemacs
  :ensure t
  :bind (("<f9>" . treemacs-select-window)
         :map treemacs-mode-map
         ("<f9>"  . treemacs-quit))
  :config
  (load "treemacs-autoloads")
  (setq treemacs-python-executable (or (executable-find "python3") (executable-find "python")))
  (setq treemacs-width 25))

;; ----------------------------------------------------------------
;; ace window
;; ----------------------------------------------------------------
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; ----------------------------------------------------------------
;; anonymous mode
;; ----------------------------------------------------------------
(use-package anonymous-mode
  :vc (:url "https://github.com/tateishi/anonymous-mode/" :rev :newest)
  :mode "\\.anon\\'"
  :custom
  (anonymous-indent-offset 4))

(provide '50-core)

;;; 50-core.el ends here
