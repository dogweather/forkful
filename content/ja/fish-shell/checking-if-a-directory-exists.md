---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Fish Shell: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

「ディレクトリが存在するかどうかをチェックする」とは、プログラマーがファイルシステム上の特定の場所に存在するかどうかを確認することを指します。このようなチェックを行う理由は、プログラムが正しく動作するために必要なファイルが存在するかどうかを確認するためです。

## 方法：

```Fish Shell```コードブロック内のコーディング例とサンプル出力を使用して、ディレクトリが存在するかどうかをチェックする方法を説明します。

ファイルパスが「```/home/user/documents```」であるディレクトリが存在するかどうかをチェックする場合は、以下のようにコマンドを入力します。

```
test -d /home/user/documents; and echo "ディレクトリが存在します。" | or echo "ディレクトリが存在しません。"
```

上記のコマンドは、まずディレクトリが存在するかどうかをチェックし、結果に応じてメッセージを出力します。ディレクトリが存在する場合は「ディレクトリが存在します。」というメッセージが表示され、存在しない場合は「ディレクトリが存在しません。」というメッセージが表示されます。

## 深堀り：

ディレクトリが存在するかどうかをチェックすることは、プログラミングでよく使用される基本的な機能の1つです。UNIXシステムでは、```test```コマンドを使用してディレクトリの存在を確認することができます。

また、```[ -d "/home/user/documents" ]```や```[[ -d "/home/user/documents" ]]```のようなシンタックスも使用することができますが、これらは実質的に```test -d```と同じ機能を持っています。

ディレクトリが存在しない場合、```test```コマンドは```1```を返します。一方、存在する場合は```0```を返します。これを利用して、プログラム内で条件分岐することができます。

## 関連情報：

- [fishshell.com - test コマンド](https://fishshell.com/docs/current/cmds/test.html)
- [UNIXコマンドリファレンス: test](http://www.cs.utah.edu/dept/old/texinfo/gawk/gawk_6.html#SEC43)
- [UNIX マニュアルページ: test](https://man7.org/linux/man-pages/man1/test.1.html)