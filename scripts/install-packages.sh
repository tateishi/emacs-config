#!/usr/bin/env bash

set -euo pipefail

EMACS=${EMACS:-emacs}           # 例: EMACS=/opt/homebrew/bin/emacsclient -a \"\" -e ...
PKGLIST=${1:-packages.txt}

# -Q: init.el を読まないクリーン環境で実行
$EMACS -Q --batch -l scripts/batch-install.el \
       --eval "(batch-install-packages \"${PKGLIST}\")"
