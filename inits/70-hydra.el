;;; 70-hydra.el --- Hydra config  -*- lexical-binding: t -*-

;; Copyright (C) 2025 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2025/08/16

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

;; Hydra config

;;; Code:

;;;
;;; hydra
;;;

(use-package hydra
  :ensure t
  :functions
  hydra-main/body
  hydra-windmove/body
  hydra--call-interactively-remap-maybe
  hydra-default-pre
  hydra-idle-message
  hydra-keyboard-quit
  hydra-set-transient-map
  hydra-show-hint
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
_j_: down line   _u_: up list
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

(provide '70-hydra)

;;; 70-hydra.el ends here
