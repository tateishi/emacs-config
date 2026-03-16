;;; 50-core.el --- My init core  -*- lexical-binding: t -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/12
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

;; My init core.

;;; Code:

(require 'use-package)

;; ----------------------------------------------------------------
;; clip board
;; ----------------------------------------------------------------
(use-package clipetty
  :if (not (or (display-graphic-p) noninteractive))
  :bind ("M-w" . clipetty-kill-ring-save))

;; ----------------------------------------------------------------
;; keyfreq
;; ----------------------------------------------------------------
(use-package keyfreq
  :if (not noninteractive)

  :init
  (make-directory (locate-user-emacs-file "keyfreq") t)
  (setq keyfreq-file (locate-user-emacs-file "keyfreq/keyfreq.el"))
  (setq keyfreq-file-lock (locate-user-emacs-file "keyfreq/keyfreq.lock"))
  (setq keyfreq-excluded-commands '(self-insert-command))

  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ----------------------------------------------------------------
;; tempel template package
;; ----------------------------------------------------------------
(use-package tempel
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert))

  :custom
  (tempel-path (list (expand-file-name "templates/*.eld" user-emacs-directory))))

(use-package tempel-collection :after tempel)

(require 'corfu-auto)
(with-eval-after-load 'corfu
  (add-to-list 'corfu-auto-commands #'tempel-complete))

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
  :custom
  (org-agenda-files (if (file-exists-p org-directory)
                        (directory-files-recursively org-directory "\\.org$")))
  (org-timestamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M"))
  (org-capture-templates
   '(("n" "Note" entry (file+headline "~/org/notes.org" "Notes") "* %?\nEntered on %U\n %i\n %a")))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))

  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c b" . my-show-org-notes-buffer))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(use-package org-journal
  :custom
  (org-journal-dir "~/org/local/journal/")
  (org-journal-date-format "%Y-%m-%d (%A)")

  :bind (("C-c j" . org-journal-new-entry)))

;; ----------------------------------------------------------------
;; undo tree
;; ----------------------------------------------------------------
(use-package undo-tree
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
  :bind
  ("C-c :" . avy-goto-char-timer))

;; ----------------------------------------------------------------
;; google translate
;; ----------------------------------------------------------------
(use-package google-translate
  :preface
  (defun my-google-translate--text-at-point ()
    (cond
     ((use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end)))
     ((thing-at-point 'sentence t))
     ((thing-at-point 'word t))
     (t "")))

  (defun my-google-translate-en->ja ()
    "英->日翻訳 リージョン優先 なければ単語/文を推定."
    (interactive)
    (google-translate-translate "en" "ja" (my-google-translate--text-at-point)))

  (defun my-google-translate-ja->en ()
    "日->英翻訳 リージョン優先 なければ単語/文を推定."
    (interactive)
    (google-translate-translate "ja" "en" (my-google-translate--text-at-point)))

  (defun my-google-translate-en<->ja (&optional reverse)
    "通常は英->日,C-u付きで日->英."
    (interactive "P")
    (if reverse
        (my-google-translate-ja->en)
      (my-google-translate-en->ja)))

  :custom
  (google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")))
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  (google-translate-output-destination 'popup)
  (google-translate-show-phonetic t)

  :bind
  (("C-c t" . my-google-translate-en<->ja)))

;; ----------------------------------------------------------------
;; smartparens
;; ----------------------------------------------------------------
(use-package smartparens
  :commands
  (smartparens-mode smartparens-strict-mode show-smartparens-mode)

  :preface
  (defun my-sp-disable-electric-pair ()
    "smartparens を使うバッファでは electric-pairを無効化する."
    (electric-pair-local-mode -1))

  :custom
  (sp-highlight-pair-overlay t)
  (sp-highlight-wrap-overlay t)

  :hook
  (prog-mode . smartparens-mode)
  (prog-mode . show-smartparens-mode)
  (prog-mode . my-sp-disable-electric-pair)
  (emacs-lisp-mode . smartparens-strict-mode)
  (lisp-mode . smartparens-strict-mode)

  :config
  (require 'smartparens-config))

;; ----------------------------------------------------------------
;; rainbow-delimiters
;; ----------------------------------------------------------------
(use-package rainbow-delimiters
  :commands
  (rainbow-delimiters-mode)

  :hook
  (prog-mode . rainbow-delimiters-mode))

;; ----------------------------------------------------------------
;; highlight-indent-guides
;; ----------------------------------------------------------------
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-character ?|))

;; ----------------------------------------------------------------
;; whitespace-mode
;; ----------------------------------------------------------------
(use-package whitespace
  :commands
  (whitespace-mode global-whitespace-mode whitespace-cleanup)

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
  :preface
  (defun my-eglot-format ()
    (when (and (bound-and-true-p eglot-managed-p)
               (eglot--server-capable :documentFormattingProvider))
      (eglot-format-buffer)))

  :custom
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)

  :commands
  (eglot eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp")))

;; ----------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------
(use-package flycheck
  :init
  (global-flycheck-mode))

;; ----------------------------------------------------------------
;; tree-sitter
;; ----------------------------------------------------------------
(use-package treesit
  :init
  ;; grammar のソース
  (setq treesit-language-source-alist
        '((c       "https://github.com/tree-sitter/tree-sitter-c")
          (cpp     "https://github.com/tree-sitter/tree-sitter-cpp")
          (python  "https://github.com/tree-sitter/tree-sitter-python")))

  :custom
  (setq treesit-font-lock-level 4))

;; ----------------------------------------------------------------
;; cc mode
;; ----------------------------------------------------------------
;; * 事前準備必要(c, c++のgrammarをインストール)
;; M-x treesit-install-language-grammar RET
;; c RET  ;; そのまま y で進める
;; M-x treesit-install-language-grammar RET
;; cpp RET  ;; そのまま y で進める
;; c/c++ 用lsp インストールが必要
;; apt install clangd -y
(defun my/use-c-ts-mode-if-available ()
  "Treesitが有効なときc-ts-mode."
  (if (treesit-language-available-p 'c)
      (c-ts-mode)
    (c-mode)))

(defun my/use-cpp-ts-mode-if-available ()
  "Treesitが有効なときc++-ts-mode."
  (if (treesit-language-available-p 'cpp)
      (c++-ts-mode)
    (c++-mode)))

(use-package cc-mode
  :mode
  (("\\.c\\'"   . my/use-c-ts-mode-if-available)
   ("\\.h\\'"   . my/use-c-ts-mode-if-available)
   ("\\.cpp\\'" . my/use-cpp-ts-mode-if-available)
   ("\\.hpp\\'" . my/use-cpp-ts-mode-if-available))

  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (c-mode-common . my-enable-trailing-whitespace))

(use-package google-c-style
  :after cc-mode
  :hook
  (c-mode-common . google-set-c-style)
  (c-mode-common . google-make-newline-indent))

;; ----------------------------------------------------------------
;; python mode
;; ----------------------------------------------------------------
;; * 事前準備必要(pythonのgrammarをインストール)
;; M-x treesit-install-language-grammar RET
;; python RET  ;; そのまま y で進める
(defun my/use-python-ts-mode-if-available ()
  "Treesitが有効なときpython-ts-mode."
  (if (treesit-language-available-p 'python)
      (python-ts-mode)
    (python-mode)))

(use-package python
  :mode
  (("\\.py\\'" . my/use-python-ts-mode-if-available))
  :hook
  (python-ts-mode . eglot-ensure)
  (python-mode . smartparens-mode))

;; ----------------------------------------------------------------
;; python-black
;; ----------------------------------------------------------------
(use-package python-black
  :after python)

;; ----------------------------------------------------------------
;; py-isort
;; ----------------------------------------------------------------
(use-package py-isort
  :after python)

;; ----------------------------------------------------------------
;; markdown-mode
;; ----------------------------------------------------------------
(use-package markdown-mode
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
  (ledger-report-auto-refresh-sticky-cursor t)
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
  (add-to-list 'ledger-report-format-specifiers '("tagname" . shiwake-report-tagname-format-specifier))
  (add-to-list 'ledger-report-format-specifiers '("month" . shiwake-report-month-format-specifier)))

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
  :commands (typescript-mode)
  :custom
  (typescript-indent-level 2))

;; ----------------------------------------------------------------
;; json
;; ----------------------------------------------------------------
(use-package json-mode
  :commands (json-mode)
  :custom
  (js-indent-level 2))

(use-package prettier-js
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
  \(use-package package-1\)
  \(use-package package-2\)\)"
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (p) `(use-package ,p)) packages)))

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
  powershell
  rust-mode
  yaml-mode)

;; ----------------------------------------------------------------
;; yasnippet
;; ----------------------------------------------------------------
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ----------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------
(use-package magit
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
  :init
  (modus-themes-select 'modus-vivendi)
  :custom
  (modus-themes-italic-constructs nil)
  (modus-themes-bold-constructs nil)
  :config
  :bind ("<f5>" . modus-themes-toggle))

;; ----------------------------------------------------------------
;; persp-mode
;; ----------------------------------------------------------------
(use-package persp-mode
  :custom
  (persp-auto-resume-time -1)
  (persp-auto-save-opt 0)
  :config
  (persp-mode t))

;; ----------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------
(use-package treemacs
  :preface
  (defun my-treemacs-detect-python ()
    "Detect a suitable Python executable for Treemacs."
    (or (executable-find "python3")
        (executable-find "python")
        (when (eq system-type 'windows-nw)
          (let ((candidates '("c:/windows/py.exe" "c:/python312/python.exe")))
            (seq-find #'file-exists-p candidates)))))

  :commands
  (treemacs treemacs-select-window)

  :bind
  (("<f9>" . treemacs)
   ("M-0" . treemacs-select-window))
  :bind
  (:map treemacs-mode-map
        ("<f9>" . treemacs-quit))

  :custom
  (treemacs-width  25)
  (treemacs-no-png-images nil)
  (treemacs-indentation 2)
  (treemacs-file-event-delay 500)

  :config
  (let ((py (my-treemacs-detect-python)))
    (when py
      (setq treemacs-python-executable py)))
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (when (executable-find "git")
    (treemacs-git-mode 'simple)))

;; ----------------------------------------------------------------
;; ace window
;; ----------------------------------------------------------------
(use-package ace-window
  :bind (("C-x o" . ace-window)))

;; ----------------------------------------------------------------
;; eyebrowse layout manager
;; ----------------------------------------------------------------
(use-package eyebrowse
  :config
  (eyebrowse-mode 1))

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
