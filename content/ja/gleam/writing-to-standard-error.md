---
title:                "標準エラーに書き込みする"
html_title:           "Gleam: 標準エラーに書き込みする"
simple_title:         "標準エラーに書き込みする"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ書き込むのか

あなたがプログラミングをしていると、コードの実行中にエラーが発生することがあります。そのエラーを見つけるためには、標準エラー出力を読む必要があります。この記事では、Gleamを使用して標準エラー出力に書き込む方法を紹介します。

## 方法

Gleamでは、`gleam/io`モジュールにある`stderr`関数を使用して標準エラー出力に書き込むことができます。以下のコードを参考にしてください。

```Gleam
import gleam/io.{stderr}

fn main() {
  stderr.print("エラーが発生しました！")
}
```

上記のコードを実行すると、標準エラー出力に`エラーが発生しました！`というメッセージが出力されます。このように、エラーが発生した箇所を特定するために、標準エラー出力にメッセージを書き込むことができます。

## ディープダイブ

標準エラー出力に書き込む際には、`stderr.write`関数を使用することもできます。この関数を使用すると、複数のメッセージを一度に書き込むことができます。以下のコードを参考にしてください。

```Gleam
import gleam/io.{stderr}

fn main() {
  stderr.write("エラーが", "発生しました！")
}
```

上記のコードを実行すると、標準エラー出力に`エラーが発生しました！`というメッセージが出力されます。`stderr.write`関数を使用することで、複数のメッセージを一度に書き込むことができ、より詳細な情報を標準エラー出力に出力することができます。

## 良く分かるリンク

- Gleamドキュメント: https://gleam.run/book/tour/io.html#standard-error
- Gleam Githubリポジトリ: https://github.com/gleam-lang/gleam/blob/master/lib/gleam/io/gleam
- スタックオーバーフロー: https://stackoverflow.com/questions/13202014/redirecting-stdout-and-stderr-to-file-in-gleam

## 参考

- Gleamドキュメント: https://gleam.run/
- Gleamコミュニティフォーラム: https://forum.gleam.run/
- Gleam公式Twitterアカウント: https://twitter.com/gleamlangjp