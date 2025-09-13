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

;;;
;;; general setting
;;;

(use-package emacs
  :preface
  (defun my-disable-trailing-whitespace ()
    "Disable `show-trailing-whitespace'."
    (setq show-trailing-whitespace nil))

  (defun my-enable-trailing-whitespace ()
    "Enable `show-trailing-whitespace'."
    (setq show-trailing-whitespace t))

  :custom
  (indent-tabs-mode nil)
  (initial-buffer-choice t)
  (require-final-newline t)
  (vc-follow-symlinks t)
  (tab-always-indent 'complete)

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
  ((before-save . delete-trailing-whitespace)))

;;;
;;; Japanese
;;;

(use-package Japanese
  :no-require t
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (setq system-time-locale "ja_JP.UTF-8"))

;;;
;;; Google mozc
;;;

(use-package mozc
  :disabled t
  :ensure t
  :custom
  (default-input-method "japanese-mozc"))

;;;
;;; DDSKK
;;;

(defun with-jisyo-dir (dir jisyo-list)
  "辞書ファイルのリストにパスを付ける.

JISYO-LISTのファイル名にDIRを付ける"
  (mapcar (lambda (e)
            (if (consp e)
                (cons (expand-file-name (car e) dir) (cdr e))
              (expand-file-name e dir)))
          jisyo-list))
(put 'with-jisyo-dir 'lisp-indent-function 1)

(use-package ddskk
  :ensure t
  :custom
  (default-input-method "japanese-skk")
  (skk-user-directory (expand-file-name "ddskk" config-dir))
  (skk-large-jisyo (expand-file-name "skk/SKK-JISYO.L" config-dir))
  (skk-extra-jisyo-file-list (with-jisyo-dir (expand-file-name "skk" config-dir)
                               '(("SKK-JISYO.jinmei" . euc-japan)
                                 ("SKK-JISYO.zipcode" . euc-japan))))
  (skk-sticky-key ";")
  :bind
  (("C-x C-j" . skk-mode))
  :config
  (setq skk-get-jisyo-directory (expand-file-name "skk" config-dir)))

;;;
;;; CJK ambiguous width chars are narrow
;;;

(when (version<= "30.1" emacs-version)
  (use-package cjk-ambiguous
    :no-require t
    :custom
    (cjk-ambiguous-chars-are-wide nil)))

;;;
;;; for microsoft windows
;;;

(use-package ms-windows
  :no-require t
  :if (eq system-type 'windows-nt)
  :config
  (setq default-process-coding-system '(utf-8-dos . cp932)))

;;;
;;; ORG mode
;;;

;;
;; helper commands/functions.
;;

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

;;
;; package definition
;;

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

;;;
;;; repeat-mode
;;;

(use-package repeat-mode
  :no-require t
  :if (version< "28" emacs-version)
  :config
  (repeat-mode))

(use-package :backup
  :no-require t
  :custom
  (backup-by-copying t)
  (backup-directory-alist (list (cons ".*" (expand-file-name "backup" config-dir))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist (list (cons ".*" (expand-file-name "undo" config-dir))))
  :config
  (global-undo-tree-mode))

;;;
;;; avy
;;;

(use-package avy
  :ensure t
  :bind
  ("C-c :" . avy-goto-char-timer))

;;;
;;; eldoc
;;;

(use-package eldoc
  :ensure t
  :hook
  (emacs-lisp-mode . turn-on-eldoc-mode)
  (lisp-interaction-mode . turn-on-eldoc-mode)
  (ielm-mode . turn-on-eldoc-mode))

;; (use-package auto-complete
;;   :config
;;   (global-auto-complete-mode t))

;;;
;;; 補完パッケージ company
;;;

(use-package company-statistics
  :ensure t
  :hook
  (after-init . company-statistics-mode))

(use-package company
  :ensure t
  :after company-statistics
  :custom
  (company-minimum-prefix-length 2)
  (company-transformers
   '(company-sort-by-statistics
     company-sort-by-occurrence
     company-sort-by-backend-importance))
  :hook
  (after-init . global-company-mode))

;;;
;;; helm
;;;

(use-package helm
  :ensure t
  :commands
  (helm-M-x helm-apropos helm-find-files helm-for-files))

;; (use-package helm
;;   :bind
;;   (("C-x C-f" . helm-find-files)
;;    ("M-x"     . helm-M-x)
;;    ("C-x b"   . helm-for-files)
;;    :map helm-map
;;    ("C-h"     . delete-backward-char)))

;;;
;;; helm-ag
;;;

;; (use-package helm-ag
;;   :ensure t
;;   :commands
;;   (helm-ag))

;; (use-package helm-ag)

;;;
;;; vertico
;;;

(use-package vertico
  :ensure t
  :custom
  (vertico-count 20)
  :config
  (vertico-mode))

;;;
;;; orderless
;;;

(use-package orderless
  :ensure t
  :defines consult--regexp-compiler
  :functions consult--highlight-regexps consult--convert-regexp consult--split-escaped migemo-get-pattern
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
  (orderless-style-dispatchers '(my-orderless-migemo-dispatcher))
  :config
  (setq consult--regexp-compiler #'my-consult--migemo-regexp-compiler))

;;;
;;; consult
;;;

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x l" . consult-line)
   ("C-x m" . consult-grep)))

;;;
;;; marginalia
;;;

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-annotators
               '(command marginalia-annotate-command builtin))
  (marginalia-mode))

;;;
;;; google translate
;;;

(use-package google-translate
  :ensure t
  :defines google-translate-translation-directions-alist
  :bind
  (("C-c t" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "ja")
                                                        ("ja" . "en")))
  )

;;;
;;; smartparens
;;;

(use-package smartparens
  :ensure t
  :custom
  (electric-pair-mode nil)
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode))

;;;
;;; rainbow-delimiters
;;;

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;
;;; highlight-indent-guides
;;;

(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-character ?|))

;;;
;;; whitespace-mode
;;;

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

;;;
;;; Eglot (emacs client of language server)
;;;

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp")))

;;;
;;; flycheck
;;;

(use-package flycheck
  :ensure t
  :hook
  (after-init-hook . global-flycheck-mode))

;;;
;;; program modes
;;;

;;;
;;; cc mode
;;;

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

;;;
;;; python mode
;;;

(use-package python-mode
  :ensure t
  :hook
  (python-mode-hook . smartparens-mode))

;;;
;;; python-black
;;;

(use-package python-black
  :ensure t
  :after python)

;;;
;;; py-isort
;;;

(use-package py-isort
  :ensure t
  :after python)

;;;
;;; markdown-mode
;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

;;;
;;; jinja2-mode
;;;

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" "\\.jinja\\'" "\\.jinja2\\'"))

;;;
;;; ledger-mode / shiwake-mode
;;;

(use-package shiwake-mode
  :load-path "lisp"
  :commands (shiwake-mode)
  :custom
  (ledger-accounts-file "~/wks/ledger/accounts/accounts.dat")
  (ledger-payees-file "~/wks/ledger/accounts/payees.dat")
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-post-amount-alignment-at :decimal)
  (ledger-complete-in-steps t)
  (ledger-reports
   '(("残高" "%(binary) -f %(ledger-file) --sort date,-amount reg %(account)")
     ("店別" "%(binary) -f %(ledger-file) --sort date,-amount reg @%(payee)")
     ("bal" "%(binary)  -f %(ledger-file) bal")
     ("bal this month" "%(binary) -f %(ledger-file)  -p %(month) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  :hook
  (ledger-mode . my-enable-trailing-whitespace))

;;;
;;; kinshu mode
;;;

(use-package kinshu-mode
  :load-path "lisp"
  :commands (kinshu-mode))

;;; web-mode
;;; url: https://web-mode.org/

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

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :custom
  (typescript-indent-level 2))

(use-package json-mode
  :ensure t
  :commands (json-mode)
  :custom
  (js-indent-level 2))

;; (mapc (lambda (package) (use-package package :ensure t))
;;       '(cmake-mode
;;         csv-mode
;;         docker-compose-mode
;;         dockerfile-mode
;;         haskell-mode
;;         meson-mode
;;         python-mode
;;         rust-mode
;;         yaml-mode))

;; (defmacro expand-package (&rest packages)
;;   (declare (indent defun))
;;   (append '(progn)
;;           (mapcar (lambda (p) `(use-package ,p :ensure t))
;;                   packages)))

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

;;;
;;; magit
;;;

(use-package magit
  :ensure t
  :bind
  (("C-x C-g" . magit-status)
   ("C-x g"   . magit-status)))

;;;
;;; migemo
;;;

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

;;;
;;; open junk file
;;;

(use-package open-junk-file
  :ensure t
  :bind
  ("C-x j" . open-junk-file)
  :custom
  (open-junk-file-format "~/.texts/%Y/%m/%Y-%m-%d-%H%M%S."))

;;;
;;; user interface
;;;

;;; modus themes
;;; url: https://gitlab.com/protesilaos/modus-themes
;;; url: https://protesilaos.com/emacs/modus-themes

(use-package modus-themes
  :ensure t
  :init
  (modus-themes-select 'modus-vivendi)
  :custom
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  :config
  :bind ("<f5>" . modus-themes-toggle))

;;;
;;; calendar mode
;;;

(use-package calendar
  :ensure t
  :custom
  (calendar-week-start-day 1)
  (calendar-latitude 35.18)
  (calendar-longitude 136.90)
  (calendar-location-name "名古屋市, 愛知県"))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package persp-mode
  :ensure t
  :config (persp-mode t))

(use-package treemacs
  :ensure t
  :bind (("<f9>" . treemacs-select-window)
         :map treemacs-mode-map
         ("<f9>"  . treemacs-quit))
  :config
  (load "treemacs-autoloads")
  (setq treemacs-python-executable (or (executable-find "python3") (executable-find "python")))
  (setq treemacs-width 25))

;;;
;;; window manipulation
;;;

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(provide '50-core)

;;; 50-core.el ends here
