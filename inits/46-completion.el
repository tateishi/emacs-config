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

;; completion setting.

;;; Code:

;; -----------------------------
;; Vertico
;; -----------------------------
(use-package vertico
  :init
  (vertico-mode))

;; -----------------------------
;; Vertico-directory
;; -----------------------------
(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; -----------------------------
;; Orderless
;; -----------------------------
;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (defun orderless-migemo-dispatcher (pattern _index _total)
;;   `(orderless-regexp . ,(migemo-get-pattern pattern)))

(defun orderless-migemo-dispatcher (pattern _index _total)
  (when (and (featurep 'migemo)
             (string-match-p "\\`[[:ascii:]]+\\'" pattern))
    `(orderless-regexp . ,(migemo-get-pattern pattern))))

(defun consult--migemo-regexp-compiler (input type ignore-case)
  (let ((migemo-regexp (mapcar #'migemo-get-pattern (consult--split-escaped input))))
    (cons (mapcar (lambda (reg) (consult--convert-regexp reg type)) migemo-regexp)
          (lambda (str) (consult--highlight-regexps migemo-regexp ignore-case str)))))

(use-package orderless
  :init
  (setq completion-styles '(orderless-migemo orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (orderless-define-completion-style orderless-migemo
    (orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefix orderless-flex))
    (orderless-style-dispatchers '(orderless-migemo-dispatcher))))

(setq orderless-matching-styles '(orderless-literal orderless-regexp))
(setq orderless-style-dispatchers '(orderless-migemo-dispatcher))

;; -----------------------------
;; Cape
;; -----------------------------
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; -----------------------------
;; Marginalia
;; -----------------------------
(use-package marginalia
  :init
  (marginalia-mode))

;; -----------------------------
;; Consult
;; -----------------------------
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)))

;; -----------------------------
;; Corfu
;; -----------------------------
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t))

;; -----------------------------
;; Corfu-Terminal
;; -----------------------------
;; 端末でも使うなら
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :init (corfu-terminal-mode))

;; -----------------------------
;; Corfu-Popupinfo
;; -----------------------------
(use-package corfu-popupinfo
  :after corfu
  :init (corfu-popupinfo-mode))

(provide '46-completion)

;;; 46-completion.el ends here
