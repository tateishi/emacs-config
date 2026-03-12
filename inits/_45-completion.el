;;; 45-completion.el --- completion setting  -*- lexical-binding: t -*-

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

;; completion setting.

;;; Code:

;; ----------------------------------------------------------------
;; 補完パッケージ company
;; ----------------------------------------------------------------
(use-package company
  :custom
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode))

(use-package company-statistics
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
  :custom
  (vertico-count 20)
  :config
  (vertico-mode))

;; ----------------------------------------------------------------
;; consult
;; ----------------------------------------------------------------
(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x l" . consult-line)
   ("C-x m" . consult-grep)
   ("C-x C-r" . consult-recent-file)))

;; ----------------------------------------------------------------
;; orderless
;; ----------------------------------------------------------------
(use-package orderless
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
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-annotators
               '(command marginalia-annotate-command builtin))
  (marginalia-mode))

(provide '45-completion)

;;; 45-completion.el ends here
