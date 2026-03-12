;;; batch-install.el --- Install/upgrade packages from a list in batch -*- lexical-binding:t; -*-

;;; Commentary:

;; 使い方（ELPA/MELPA パッケージだけの場合）：
;;   emacs -Q --batch -l batch-install.el --eval "(batch-install-packages \"packages.txt\")"
;;
;; packages.txt は 1行1エントリで、空行と # で始まる行は無視します。
;; 例:
;;   # elpa/melpa packages
;;   magit
;;   consult
;;
;; 追加機能（任意）：VC パッケージ行
;;   vc <name> <url> [rev]
;; 例:
;;   vc shiwake-mode https://github.com/tateishi/shiwake-mode/ newest
;;
;; 終了コード：
;;   0 … 成功
;;   1 … 失敗（ネットワーク/アーカイブ取得失敗/インストール失敗など）

;;; Code:

(require 'package)
(require 'subr-x)  ;; string-trim, string-empty-p

;; ------- リポジトリ設定（必要に応じて調整） -------
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu"     . 100)
        ("nongnu"  . 90)
        ("melpa"   . 50)))

;; ------- 初期化 -------
(unless package--initialized
  (package-initialize))

;; ------- ユーティリティ：ファイル読み取り -------
(defun batch--read-entries (file)
  "FILE から空行と # 行を除いた「行文字列」のリストを返す。"
  (with-temp-buffer
    (insert-file-contents file)
    (let (acc)
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line))
            (push line acc)))
        (forward-line 1))
      (nreverse acc))))

(defun batch--ensure-archives (&optional update)
  "パッケージアーカイブを確実に更新。失敗時は kill-emacs 1。"
  (message "[batch] refreshing package archives...")
  (condition-case err
      (progn
        (unless package-archive-contents
          (package-refresh-contents))
        ;; すでに取得済みでも最新へ更新したい場合は常に refresh したいなら下を有効化：
        (when update
          (package-refresh-contents))
        (message "[batch] archives ready"))
    (error
     (message "[batch] archive refresh failed: %S" err)
     (kill-emacs 1))))

;; ------- package.el 由来の install/upgrade -------
(defun batch--pkg-installed-desc (pkg)
  "PKG のインストール済み package-desc（最新）を返す。なければ nil。"
  (car (cdr (assq pkg package-alist))))

(defun batch--pkg-archive-desc (pkg)
  "PKG のアーカイブ側 package-desc（最新）を返す。なければ nil。"
  (car (cdr (assq pkg package-archive-contents))))

(defun batch--install-or-upgrade-elpa (pkg)
  "PKG（シンボル）をインストールまたはアップグレードする（ELPA/MELPA）。"
  (let* ((inst-desc  (batch--pkg-installed-desc pkg))
         (avail-desc (batch--pkg-archive-desc pkg)))
    (cond
     ;; 未インストール
     ((null inst-desc)
      (message "[batch] install     %s" pkg)
      (package-install pkg))

     ;; インストール済みで、アーカイブに新しい版がある
     ((and avail-desc
           (version-list-< (package-desc-version inst-desc)
                           (package-desc-version avail-desc)))
      (message "[batch] upgrade     %s  %s -> %s"
               pkg
               (package-version-join (package-desc-version inst-desc))
               (package-version-join (package-desc-version avail-desc)))
      ;; ここは package-install でもよいが、desc を渡すと確実
      (package-install avail-desc))

     ;; 何もすることがない
     (t
      (message "[batch] up-to-date  %s (%s)"
               pkg
               (package-version-join (package-desc-version inst-desc)))))))

;; ------- VC パッケージの install/upgrade（任意） -------
;; use-package の :vc は内部的に package-vc を使い、:rev :newest で最新コミットを選べます。[3](https://www.reddit.com/r/emacs/comments/epxdoz/replace_eyebrowse_with_tab_bar_mode/)[4](https://qiita.com/ayatakesi/items/94a578172cc582531443)
;; package-vc-install で入れたパッケージは通常のパッケージと同様に upgrade できます。[1](https://qiita.com/grugrut/items/1df4bdbe1453f4b06fc2)[2](https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-02/msg02307.html)
(defun batch--install-or-upgrade-vc (name url &optional rev)
  "VC から NAME を URL で install/upgrade。REV は 'newest など。"
  (require 'package-vc nil t) ;; Emacs 29+ にある想定
  (unless (featurep 'package-vc)
    (error "package-vc is not available in this Emacs"))
  (let* ((pkg (intern name))
         (rev-arg (cond
                   ((null rev) :newest)
                   ((member rev '("newest" ":newest")) :newest)
                   (t rev))))
    (message "[batch] vc install/upgrade %s from %s (rev=%s)" name url rev-arg)
    ;; package-vc-install は spec でも呼べる（:url など）[2](https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-02/msg02307.html)
    ;; 上書き確認をスキップするために yes-or-no-p を常に t を返す関数に置き換える
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t)))
       (package-vc-install `(,pkg :url ,url :rev ,rev-arg)))))

;; ------- エントリ解析 -------
(defun batch--dispatch-entry (line)
  "LINE（1行分）を解析して適切な処理を返す。
返り値は (TYPE . ARGS) の形。
TYPE: 'elpa or 'vc"
  (let* ((parts (split-string line "[ \t]+" t)))
    (cond
     ;; VC 行: vc <name> <url> [rev]
     ((and (>= (length parts) 3)
           (string= (car parts) "vc"))
      (let ((name (nth 1 parts))
            (url  (nth 2 parts))
            (rev  (nth 3 parts)))
        (cons 'vc (list name url rev))))

     ;; 通常: パッケージ名
     (t
      (cons 'elpa (list (intern (car parts))))))))

;;;###autoload
(defun batch-install-packages (file &optional update)
  "FILE（1行1エントリ）に従い一括で install/upgrade を行う。"
  (let* ((lines (batch--read-entries file)))
    (message "[batch] entries: %S" lines)
    ;; ELPA/MELPA を使う可能性があるならアーカイブ更新
    (batch--ensure-archives update)
    (dolist (line lines)
      (let* ((spec (batch--dispatch-entry line))
             (type (car spec))
             (args (cdr spec)))
        (condition-case err
            (pcase type
              ('elpa (batch--install-or-upgrade-elpa (car args)))
              ('vc   (apply #'batch--install-or-upgrade-vc args))
              (_ (error "Unknown entry type: %S" type)))
          (error
           (message "[batch] ERROR on '%s': %S" line err)
           (kill-emacs 1))))))
  (message "[batch] done")
  (kill-emacs 0))

;;; batch-install.el ends here
