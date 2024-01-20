---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイルを読むとは、その内容を一行ずつ（またはまとめて）読み取るプロセスのことです。これは、設定のロード、ログファイルの解析、ユーザー入力の処理などでプログラマーにとって不可欠なタスクです。

## やり方:

以下にテキストファイルを読む方法の例を示します。Bashでの最も一般的なアプローチはwhileループとreadコマンドを使用するものです。

```Bash
while IFS= read -r line
do
  echo "$line"
done < "yourfile.txt"
```

このコードは、"yourfile.txt"というファイルを一行ずつ読み、各行を表示します。

## ディープダイブ:

Bashは、1979年に初めてUnixシェルとして登場し、テキストファイルの読み取りは当初から重要な機能でした。他の代替手段としては、「cat」コマンドを利用する方法や、「awk」や「sed」のようなパワフルなテキスト処理ツールを使用する方法があります。

詳細については、Bashのマニュアルページを参照してください。readコマンドは、指定した区切り文字（デフォルトでは改行）でテキストを切り分けます。IFS（Internal Field Separator）は、この区切り文字を変更するための変数です。

## 関連情報:

1. Bashマニュアル: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
2. Readコマンドの詳細: [https://linux.die.net/man/1/read](https://linux.die.net/man/1/read)
3. AWK, SEDなどの詳細: [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html) , [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)