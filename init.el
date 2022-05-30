;;;; init.el --- -*- lexical-binding: t -*-


(setq gc-cons-threshold (* 1024 1024 1024))
(defvar warning-minimum-level :emergency)

(defvar config-dir (file-name-directory load-file-name)
  "The root dir of the Emacs config.")

(setq custom-file (expand-file-name "custom.el" config-dir))
(if (file-exists-p custom-file) (load custom-file))


(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(let ((package 'use-package))
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

(use-package init-loader
  :ensure t
  :functions init-locader-load
  :init (setq init-loader-byte-compile t)
  :config (init-loader-load))
