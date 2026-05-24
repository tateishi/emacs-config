;; -*- flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

;; user-local.example.el
;; このファイルを ~/.config/emacs/user-local.el としてコピー＆編集して使ってください。
(setq user-full-name "YOUR NAME")
(setq user-mail-address "you@example.com")

;; packageのロードにeaskを使うとき下記の変数を t にする
(defvar my-using-eask-for-package nil
  "eask で package をロードしているとき t それ以外 nil とする.")
