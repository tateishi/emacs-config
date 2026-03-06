$ErrorActionPreference = "Stop"
$Emacs   = $env:EMACS
if (-not $Emacs) { $Emacs = "$env:LOCALAPPDATA\Emacs\bin\emacs.exe" }   # 必要に応じてフルパス
$PkgList = if ($args.Length -gt 0) { $args[0] } else { "packages.txt" }

& $Emacs -Q --batch -l scripts/batch-install.el `
   --eval "(batch-install-packages \`"$PkgList\`")"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
