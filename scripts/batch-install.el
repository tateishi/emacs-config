;;; batch-install.el --- Install/upgrade packages from a list in batch -*- lexical-binding:t; -*-

;;; Commentary:

;; 使い方：
;;   emacs -Q --batch -l batch-install.el --eval "(batch-install-packages \"packages.txt\")"
;;
;; 返り値（プロセス終了コード）：
;;   0 … 成功（何らかの処理をしていなくても成功）
;;   1 … 失敗（ネットワークやアーカイブ取得失敗など）

;;; Code:

(require 'package)

;; ------- リポジトリ設定（必要に応じて調整） -------
;; GNU ELPA を先頭、MELPA を後方。順序は “衝突時の優先度” に影響します。
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; TLS 設定（古い環境でのエラー回避策を仕込みたいときはここに追加）

;; ------- 初期化 -------
;; - 念のため初期化（-Q のため package-initialize は不要なことが多いが、古い環境対策）
(unless package--initialized
  (package-initialize))

;; ------- ユーティリティ -------
(defun batch--non-empty-lines (file)
  "FILE から空行と # で始まる行を除いたリストを返す。"
  (with-temp-buffer
    (insert-file-contents file)
    (let (acc)
      (while (not (eobp))
        (let* ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line))
            (push line acc)))
        (forward-line 1))
      (nreverse acc))))

(defun batch--ensure-archives ()
  "パッケージアーカイブを確実に更新。失敗時はエラーを投げる。"
  (message "[batch] refreshing package archives...")
  (condition-case err
      (progn
        (unless package-archive-contents
          (package-refresh-contents))
        ;; すでに取得済みでも最新へ更新したい場合は常に refresh したいなら下を有効化：
        ;; (package-refresh-contents)
        (message "[batch] archives ready"))
    (error
     (message "[batch] archive refresh failed: %S" err)
     (kill-emacs 1))))

(defun batch--install-or-upgrade (pkg)
  "PKG（シンボル）をインストールまたはアップグレード。"
  (let* ((desc (cadr (assoc pkg package-alist))) ; インストール済みの最新版
         (inst (and desc (package-desc-name desc)))
         (avail (cadr (assoc pkg package-archive-contents)))) ; アーカイブの最新版
    (cond
     ;; 未インストール → インストール
     ((not inst)
      (message "[batch] install  %s" pkg)
      (package-install pkg))

     ;; インストール済み & アップグレードあり → アップグレード
     ((and avail
           (version-list-<
            (package-desc-version desc)
            (package-desc-version avail)))
      (message "[batch] upgrade  %s  %s → %s"
               pkg
               (package-version-join (package-desc-version desc))
               (package-version-join (package-desc-version avail)))
      (package-install avail))

     ;; 何もすることがない
     (t
      (message "[batch] up-to-date %s (%s)"
               pkg (package-version-join (package-desc-version (or desc avail))))))))

;;;###autoload
(defun batch-install-packages (file)
  "FILE（1行1パッケージ名）に従い一括でインポート/アップグレード。"
  (let* ((pkgs (mapcar #'intern (batch--non-empty-lines file))))
    (message "[batch] target packages: %S" pkgs)
    (batch--ensure-archives)
    (dolist (p pkgs)
      (condition-case err
          (batch--install-or-upgrade p)
        (error
         (message "[batch] ERROR on %s: %S" p err)
         (kill-emacs 1)))))
  (message "[batch] done")
  (kill-emacs 0))

;;; batch-install.el ends here
