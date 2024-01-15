---
title:                "テキストファイルの書き方"
html_title:           "Gleam: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書く理由はたくさんありますが、主な理由は以下の2つです。まず、コンピューターが理解できるように情報を整理することができます。そして、長いコードを書くよりも、テキストファイルはより簡単で迅速な方法で情報を共有することができます。

## 使い方

まず、 `Gleam.FileSystem` モジュールをインポートします。次に、 `write` 関数を使用してファイルを作成し、 `write_line` 関数を使用してテキストを書き込みます。最後に、ファイルを閉じて変更を保存します。

```Gleam
import Gleam.FileSystem

let file = FileSystem.write("filename.txt")
FileSystem.write_line(file, "Hello, world!")
FileSystem.close(file)
```

出力される `filename.txt` ファイルには、"Hello, world!"というテキストが含まれています。

## 詳細

テキストファイルを書くには、いくつかの注意すべき点があります。まず、ファイルを開いたら必ず閉じるようにしましょう。また、テキストファイルはプレーンテキスト形式なので、フォーマットが崩れないように注意して文章を書く必要があります。

## はてな

テキストファイルの書き方については、ほかにもたくさんのリソースがあります。ぜひ以下のリンクも参考にしてみてください。

- [Gleam公式ドキュメント - ファイルの作成と読み書き](https://gleam.run/articles/files)
- [デジタルハリウッドの「はじめてのプログラミング」コース](https://www.dhw.co.jp/program/android.html)