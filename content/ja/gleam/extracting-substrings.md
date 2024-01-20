---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

部分文字列の抽出は、文字列全体から特定の範囲の文字を取得するプロセスです。プログラマーはこれを使用してデータの特定の部分を操るために使用します。

## 使い方：

以下は、Gleam言語で部分文字列を抽出する簡単な例です：

```Gleam
import gleam/string

fn main() {
  let example_string = "こんにちは、世界"
  let substring = string.slice(example_string, 0, 2)
  io.println(substring)
}
```

これは`こ`と出力します。

## ディープダイブ：

部分文字列の抽出は、古典的なプログラミングの技術で、多くの言語に組み込まれています。提供される機能によっては、開始インデックスと終了インデックスの指定、あるいは特定の文字、パターン、正規表現を指定することで部分文字列を取得できます。

Gleamでは、`string.slice/3`関数を使用して部分文字列を抽出します。この関数は、始点と終点のインデックスを指定することで部分文字列を返します。ただし、Gleamは0から数え始めますので注意が必要です。

もし、代替手段が必要なら、`string.split/2`を使って文字列を特定のデリミタで分割し、その結果を用いることもできます。

## 参考情報：

より詳しい情報については以下のリンクをご覧ください：

Gleamの公式文書: [Gleam string API](https://hexdocs.pm/gleam_stdlib/gleam/string.html)

部分文字列の抽出についての一般的な情報: [Wikipedia Substring article](https://en.wikipedia.org/wiki/Substring)