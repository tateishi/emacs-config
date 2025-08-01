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

(use-package startup
  :no-require t
  :config
  (setq initial-buffer-choice t))

(use-package misc
  :no-require t
  :config
  (setq-default indent-tabs-mode nil)
  (setq vc-follow-symlinks t))

(use-package Japanese
  :no-require t
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix))

(use-package mozc
  :ensure t
  :config
  (setq default-input-method "japanese-mozc"))

(use-package appearance
  :no-require t
  :config
  (column-number-mode t)
  (size-indication-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tab-bar-mode 1))

;;;
;;; for microsoft windows
;;;

(use-package ms-windows
  :no-require t
  :if (eq system-type 'windows-nt)
  :config
  (setq default-process-coding-system '(utf-8-dos . cp932)))

;;;
;;; edit
;;;

(defun my-disable-trailing-whitespace ()
  "Disable `show-trailing-whitespace'."
  (setq show-trailing-whitespace nil))

(defun my-enable-trailing-whitespace ()
  "Enable `show-trailing-whitespace'."
  (setq show-trailing-whitespace t))

(use-package general
  :no-require t
  :hook ((before-save . delete-trailing-whitespace))
  :config
  (show-paren-mode t)
  (setq-default require-final-newline t))

;;;
;;; repeat-mode
;;;

(use-package repeat-mode
  :no-require t
  :if (version< "28" emacs-version)
  :config (repeat-mode 1))

(use-package :backup
  :no-require t
  :config
  (setq backup-by-copying t)
  (setq backup-directory-alist (list (cons ".*" (expand-file-name "backup" config-dir))))
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq version-control t))

(use-package undo-tree
  :ensure t
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist (list (cons ".*" (expand-file-name "undo" config-dir)))))

(use-package eldoc
  :ensure t
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)
         (lisp-interaction-mode-hook . turn-on-eldoc-mode)
         (ielm-mode-hook . turn-on-eldoc-mode)))

;; (use-package auto-complete
;;   :config
;;   (global-auto-complete-mode t))


(defun my-company-mode-hook ()
  "Hook for company-mode."
  (setq company-minimum-prefix-length 2))

(use-package company
  :ensure t
  :hook
  (company-mode-hook . my-company-mode-hook)
  :config
  (global-company-mode))

(use-package avy
  :ensure t
  :bind
  ("C-c :" . avy-goto-char-timer))

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

(use-package helm-ag
  :ensure t
  :commands
  (helm-ag))

;; (use-package helm-ag)

;;;
;;; vertico
;;;

(use-package vertico
  :ensure t
  :config
  (setq vertico-count 20)
  (vertico-mode))

;;;
;;; orderless
;;;

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;;;
;;; consult
;;;

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer))
)

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
  :delight
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (electric-pair-mode nil)
  :config
  (require 'smartparens-config))

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
  :delight
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
  :demand t
  :delight
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
  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "Black"
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background "Black"
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background "Black"
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background "Black"))

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
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))


;;;
;;; program modes
;;;

;;;
;;; cc mode
;;;

(defun my-cc-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent)
  (setq c-basic-offset 4))

(use-package cc-mode
  :ensure t
  :init
  (use-package google-c-style :ensure t)
  :hook
  (c-mode-common . my-cc-mode-hook)
  (c-mode-common . my-enable-trailing-whitespace))

;;;
;;; python mode
;;;

(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook #'smartparens-mode)
  )

;;;
;;; markdown-mode
;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;
;;; jinja2-mode
;;;

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" "\\.jinja\\'" "\\.jinja2\\'"))

;;;
;;; ledger-mode / shiwake-mode
;;;

(defun my-ledger-mode-hook ()
  (setq-local tab-always-indent 'complete)
  (setq-local completion-cycle-threshold t)
  (setq-local ledger-complete-in-steps t))

(use-package shiwake-mode
  :load-path "lisp"
  :commands (shiwake-mode)
  :custom
  (ledger-accounts-file "~/wks/ledger/accounts/accounts.dat")
  (ledger-payees-file "~/wks/ledger/accounts/payees.dat")
  :init
  (setq ledger-reports
        '(("残高" "%(binary) -f /home/ubuntu/wks/ledger/ledger_kakei/main.ledger --sort date,-amount reg %(account)")
          ("残高忠利" "%(binary) -f /home/ubuntu/wks/ledger/ledger_tadatoshi/main.ledger --sort date,-amount reg %(account)")
          ("残高花子" "%(binary) -f /home/ubuntu/wks/ledger/ledger_hanako/main.ledger --sort date,-amount reg %(account)")
          ("店別" "%(binary) -f /home/ubuntu/wks/ledger/ledger_kakei/main.ledger --sort date,-amount reg @%(payee)")
          ("bal" "%(binary) -f /home/ubuntu/wks/ledger/ledger_kakei/main.ledger bal")
          ("bal this month" "%(binary) -f /home/ubuntu/wks/ledger/ledger_kakei/main.ledger -p %(month) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)")))

  :hook
  (ledger-mode . my-ledger-mode-hook)
  (ledger-mode . my-enable-trailing-whitespace))

;;; web-mode
;;; url: https://web-mode.org/

(defun my-web-mode-hook ()
  "Hook for web-mode"
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-engine-detection t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))

(use-package web-mode
  :ensure t
  :commands (web-mode)
  :mode (("\\.html\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :hook (web-mode . my-web-mode-hook))


(defun my-typescript-mode-hook ()
  "Hook for typescript-mode"
  (setq typescript-indent-level 2))

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :hook (typescript-mode . my-typescript-mode-hook))


(defun my-json-mode-hook ()
  "Hook for json-mode"
  (setq js-indent-level 2
        json-reformat:indent-width 2))

(use-package json-mode
  :ensure t
  :commands (json-mode)
  :hook (json-mode . my-json-mode-hook))

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

(defmacro expand-package (&rest packages)
  (declare (indent defun))
  (append '(progn)
          (mapcar (lambda (p) `(use-package ,p :ensure t))
                  packages)))

(expand-package
  ansible
  cmake-mode
  csv-mode
  docker-compose-mode
  dockerfile-mode
  haskell-mode
  ledger-mode
  meson-mode
  rust-mode
  yaml-mode)


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

(when (executable-find "cmigemo")
  (defvar config-migemo/dir "/usr/share/cmigemo/utf-8")
  (use-package migemo
    :ensure t
    :custom
    (migemo-directory
     (if (file-directory-p config-migemo/dir)
         config-migemo/dir
       (format "%s/dict/utf-8" (directory-file-name (file-name-directory (or (executable-find "cmigemo") ""))))))
    :functions (migemo-init)
    :config
    (migemo-init)))

;;;
;;; open junk file
;;;

(use-package open-junk-file
  :ensure t
  :bind
  ("C-x j" . open-junk-file)
  :config
  (setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S."))

;;;
;;; user interface
;;;

;;; modus themes
;;; url: https://gitlab.com/protesilaos/modus-themes
;;; url: https://protesilaos.com/emacs/modus-themes

(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  (modus-themes-select 'modus-vivendi)

  :config
  ;; Load the theme of your choice:
  :bind ("<f5>" . modus-themes-toggle))

;;;
;;; calendar mode
;;;

(defun my-calendar-mode-hook ()
  (setq calendar-week-start-day 1)
  (setq calendar-latitude 35.18)
  (setq calendar-longitude 136.90)
  (setq calendar-location-name "名古屋市, 愛知県"))


(use-package calendar
  :ensure t
  :hook (calendar-mode . my-calendar-mode-hook))

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

;;;
;;; key bind
;;;

(use-package key-bind
  :no-require t
  :bind
  (("C-h" . backward-delete-char-untabify)
   ("C-z" . scroll-down-command)
   ("C-c c" . compile)
   ("C-x t t" . tab-bar-switch-to-tab)
   ("<C-return>" . other-window)
   ("<M-return>" . other-frame)
   ("C-c o"   . swap-first-window)))

;;;
;;; hydra
;;;

(use-package hydra
  :ensure t
  :functions hydra-main/body hydra-windmove/body
  :bind (("C-c h"   . hydra-main/body)
         ("M-m"     . hydra-main/body)
         ("M-<SPC>" . hydra-main/body)
         ("C-c m"   . hydra-move/body)))

(use-package hydra-main :no-require t
  :after hydra
  :init
  (defhydra hydra-main (:idle 0 :hint nil)
    "
MAIN
--------------------------------------------------------------------------------
_h_: left char   _f_: forward sexp     _m_: MOVE        _0_: treemacs
_l_: right char  _b_: backward sexp    _w_: ace-window
_j_: down line   _u_: up list          _g_: helm-ag
_k_: up line     _U_: backward up list _:_: goto-char
_+_: larger      _d_: down list        _._: tag
_-_: smaller     _x_: helm-M-x         _,_: back
_q_: exit        _o_: helm-for-files
               _a_: helm-apropos
"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("h" backward-char)
    ("l" forward-char)
    ("j" next-line)
    ("k" previous-line)
    ("f" forward-sexp)
    ("b" backward-sexp)
    ("F" forward-list)
    ("B" backward-list)
    ("u" up-list)
    ("U" backward-up-list)
    ("d" down-list)
    ("A" beginning-of-defun)
    ("E" end-of-defun)
    ("x" helm-M-x)
    ("o" helm-for-files)
    ("a" helm-apropos)
    ("g" helm-ag)
    ("w" ace-window)
    (":" avy-goto-char-timer)
    ("." xref-find-definitions)
    ("," xref-go-back)
    ("m" hydra-move/body        :exit t)
    ("0" treemacs-select-window :exit t)
    ("q" nil                    :exit t)))

(use-package hydra-move :no-require t
  :after hydra
  :init
  (defhydra hydra-move (:idle 0)
    "
MOVE
----------------------------------------------------------------
_h_: left char   _a_: beginning of line
_l_: right char  _e_: end of line
_j_: down line   _g_: goto line
_k_: up line     _:_: avy-goto-char
_H_: left word   _/_: search forward
_L_: right word  _?_: search backward

_q_: exit
"
    ("h" backward-char          "backward-char")
    ("l" forward-char           "forward-char")
    ("j" next-line              "next-line")
    ("k" previous-line          "previous-line")
    ("H" backward-word          "backward-word")
    ("L" forward-word           "forward-word")
    ("a" move-beginning-of-line "bol")
    ("e" move-end-of-line       "eol")
    ("g" goto-line              "goto-line")
    (":" avy-goto-char-timer    "avy-goto-char")
    ("/" isearch-forward        "isearch-forward")
    ("\?" isearch-backward      "isearch-backward")
    ("q" nil                    "exit")))

(use-package hydra-windmove :no-require t
  :after (hydra)
  :init
  (defhydra hydra-windmove (:idle 0)
    "
WINDMOVE
----------------------------------------------------------------
_h_: left  _<return>_: select
_l_: right
_j_: down
_k_: up
_q_: exit
"
    ("h" windmove-left)
    ("l" windmove-right)
    ("j" windmove-down)
    ("k" windmove-up)
    ("<return>" swap-first-window)
    ("q" nil)))

(defun my-kinshu-mode-hook ()
  (setq indent-tabs-mode nil))

(use-package kinshu-mode
  :load-path "lisp"
  :commands (kinshu-mode)
  :hook (kinshu-mode . my-kinshu-mode-hook))

;;; 50-core.el ends here
