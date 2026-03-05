;;; early-init.el --- Early init for faster, cleaner startup -*- lexical-binding: t -*-

;; Copyright (C) 2026  TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/03/03

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
;; Emacs 27+ loads this file before creating the first frame and before init.el.
;; 目的: 起動高速化・不要な UI の抑制・package 初期化の完全制御・警告ノイズ削減。

;;; Code:

;; ----------------------------------------------------------------
;; 警告レベル
;; ----------------------------------------------------------------
(setq warning-minimum-level :error)

;; ----------------------------------------------------------------
;; GC (起動時 256MB, 起動後 64MB)
;; ----------------------------------------------------------------
(setq gc-cons-threshold (* 256 1024 1024))
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))
            (setq gc-cons-percentage 0.1)))

;; ----------------------------------------------------------------
;; 大量ロード時のオーバーヘッドを下げる
;; ----------------------------------------------------------------
(defvar early--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist early--file-name-handler-alist)
            (makunbound 'early--file-name-handler-alist)))

;; ----------------------------------------------------------------
;; load-path
;; ----------------------------------------------------------------
(let ((dir (locate-user-emacs-file "lisp")))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

;; ----------------------------------------------------------------
;; パッケージ初期化の制御
;; ----------------------------------------------------------------
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; ----------------------------------------------------------------
;; 新しい方の elc/el を優先（古いバイトコードを無視してトラブル減）
;; ----------------------------------------------------------------
(setq load-prefer-newer t)

;; ----------------------------------------------------------------
;; UI の初期状態（フレーム作成前に抑止してチラつきを防止）
;; ----------------------------------------------------------------
;; ここで無効化しておくと一瞬も表示されない。
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; ピクセル単位でのリサイズ精度向上（レイアウトの揺れ防止）。
(setq frame-resize-pixelwise t)
(push '(resize-pixelwise . t) default-frame-alist)

;; 初回レイアウト時の再計算を抑制（表示チラつき対策）。
(setq frame-inhibit-implied-resize t)

;; スプラッシュ/メッセージ非表示
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

;; 初期フレームの軽量化（必要に応じてコメント解除）
(push '(internal-border-width . 0) default-frame-alist)
;;(push '(undecorated . t)            default-frame-alist) ; OS により無視されることあり

;; ----------------------------------------------------------------
;; ネイティブコンパイル（Emacs 28+）の警告を静かに
;; ----------------------------------------------------------------
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; macOS NS ポートの余計なフルスクリーンアニメ等が気になる場合の例：
;; (when (eq system-type 'darwin)
;;   (setq ns-use-thin-smoothing t))

;; Windows でのプロセス起動の高速化（必要に応じて）
;; (when (eq system-type 'windows-nt)
;;   (setq w32-pipe-read-delay 0
;;         w32-pipe-buffer-size (* 64 1024)))

;; ----------------------------------------------------------------
;; 端末/GUI 共通のノイズ削減（早い段階で）
;; ----------------------------------------------------------------
;; 初回 redraw 最適化（古い端末で効果、GUI では限定的）
(setq inhibit-compacting-font-caches t)

;; 余計な自動生成ファイルのロード抑止（site-run-file が遅い環境向け、通常は不要）
;; (setq site-run-file nil)

(provide 'early-init)
;;; early-init.el ends here
