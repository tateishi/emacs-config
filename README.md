# emacs-config
emacs config with use-package

## 初期化

### 準備

1. package.elを使う方法
2. eask を使う方法

上記のどちらかを選択する

### package.el を使う方法

下記のコマンドでpackageをインストールする。

```bash
bash script/install.sh
```

user-local-example.el を user-local.el にコピーして ```user-full-name```などを設定するときは、```my-using-eask-for-package```は```nil```を設定する。


### eask を使う方法

下記の手順でpackageをインストールする。
user-local-example.el を user-local.el にコピーし、```my-using-eask-for-package``` を ```t``` に設定する。

#### node をインストール

- nvm を使う場合
```bash
nvm install 24
nvm use 24
```

#### Eask をインストール

```bash
npm install -g @emacs-eask/cli
```

### packageのインストール

```bash
cd ~/.config/emacs/
eask install
```
