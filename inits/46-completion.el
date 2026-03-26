;;; 46-completion.el --- completion setting  -*- lexical-binding: t -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/03/12
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

;; Modern completion configuration
;; - Vertico (+ directory)
;; - Orderless + Migemo dispatcher
;; - Consult + Migemo regexp compiler
;; - Corfu (+ popupinfo + terminal)
;; - Marginalia
;; - Cape
;; - Embark

;;; Code:

(require 'use-package)

;; ----------------------------------------------------------------
;; Vertico
;; ----------------------------------------------------------------
(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

;; ----------------------------------------------------------------
;; Orderless
;; ----------------------------------------------------------------
(require 'migemo)

(defun orderless-migemo-dispatcher (pattern _index _total)
  "Return migemo regexp for PATTERN when ascii-only and migemo available."
  (when (and (featurep 'migemo)
             (string-match-p "\\`[[:ascii:]]+\\'" pattern))
    `(orderless-regexp . ,(migemo-get-pattern pattern))))

(use-package orderless
  :custom
  (completion-styles '(orderless-migemo orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (orderless-component-separator "[-_ /.:]")

  :config
  (orderless-define-completion-style orderless-migemo
    (orderless-matching-styles
     '(orderless-literal orderless-regexp orderless-prefixes orderless-flex))
    (orderless-style-dispatchers '(orderless-migemo-dispatcher))))

;; ----------------------------------------------------------------
;; Cape
;; ----------------------------------------------------------------
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ----------------------------------------------------------------
;; Marginalia
;; ----------------------------------------------------------------
(use-package marginalia
  :init
  (marginalia-mode))

;; ----------------------------------------------------------------
;; Consult
;; ----------------------------------------------------------------
(require 'consult)

(defun consult--migemo-regexp-compiler (input type ignore-case)
  "Migemo-aware regexp compiler for Consult.
See `consult--compile-regexp' for INPUT, TYPE and IGNORE-CASE."
  (let ((migemo-regexp (mapcar #'migemo-get-pattern (consult--split-escaped input))))
    (cons (mapcar (lambda (reg) (consult--convert-regexp reg type)) migemo-regexp)
          (lambda (str) (consult--highlight-regexps migemo-regexp ignore-case str)))))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("C-x m" . consult-ripgrep)
   ("C-x C-r" . consult-recent-file)
   ("M-y" . consult-yank-from-kill-ring))

  :config
  (setq consult--regexp-compiler #'consult--migemo-regexp-compiler))

;; ----------------------------------------------------------------
;; Embark
;; ----------------------------------------------------------------
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :custom
  (embark-quit-after-action '((kill-buffer . t) (t . nil)))

  :bind
  (("C-x ." . embark-act)
   ("C-x ;" . embark-dwim)
   ("<F1> B" . embark-bindings))

  :hook
  (embark-pre-action . minibuffer-hide-completions))

(use-package embark-consult
  :after
  (embark consult)

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ----------------------------------------------------------------
;; Corfu
;; ----------------------------------------------------------------
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t))

;; Terminal support
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :init
  (corfu-terminal-mode))

;; Popup info
(use-package corfu-popupinfo
  :after corfu
  :init
  (corfu-popupinfo-mode))

(provide '46-completion)

;;; 46-completion.el ends here
