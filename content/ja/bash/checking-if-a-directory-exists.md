---
title:                "ディレクトリが存在するかチェックする"
html_title:           "Bash: ディレクトリが存在するかチェックする"
simple_title:         "ディレクトリが存在するかチェックする"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかの確認とは、ファイルシステムに特定のディレクトリが存在するかどうかを確認する過程のことです。これを行うと、プログラマーはエラーを防ぎ、プログラムが順調に進行することが可能となります。

## 手順：

ディレクトリが存在するかを確認する最も基本的な方法を以下のBashコードで示します。このコードは特定のディレクトリ（ここでは"/tmp/test"）が存在するか確認します。

```Bash
if [ -d "/tmp/test" ] 
then
    echo "Directory Exists"
else
    echo "Directory Not Exists"
fi
```

このコードを実行すると以下のような出力が得られます（"/tmp/test"が存在しない場合）。

```
Directory Not Exists
```

## ディープダイブ：

これがシェルスクリプトの標準テスト演算子 `-d` で、Unix時代から存在します。これはディレクトリが存在するかどうかをチェックするための極めて信頼性の高い方法です。

代わりに `[[ -d "/path/to/dir" ]]` を使用することも可能です。これは新しいシンタックスで、結果を逆にする (`!`) または複数のテストを組み合わせる (`&&`、`||`) など、より高度な操作が可能です。

Bashには、ディレクトリをチェックするための他の方法もあります。例えば、 `stat` コマンドを利用する方法などです。しかし、 `-d` 演算子を使用することが一般的です。

## その他の参考資料：

1. Official Bash Manual: https://www.gnu.org/software/bash/manual/bash.html
2. Unix Test Operators: https://www.tutorialspoint.com/unix/unix-test-command.htm
3. Bash Beginners Guide: http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html
4. Stat Command in Linux: https://www.tecmint.com/stat-command-in-linux/