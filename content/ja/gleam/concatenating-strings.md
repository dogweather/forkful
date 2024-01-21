---
title:                "文字列の連結"
date:                  2024-01-20T17:34:35.767729-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

文字列を連結（concatenating strings）するとは、複数の文字列を一つに結合することです。プログラマーは、データ表示、メッセージ構築、動的コンテンツ生成のためにこれをよく使います。

## How to: (やり方：)

```Gleam
import gleam/string

// 文字列の連結
fn main() {
  let greeting = "こんにちは、"
  let name = "太郎さん"
  let message = string.append(greeting, name)
  message
}

// 出力: "こんにちは、太郎さん"
```

## Deep Dive (深掘り)

文字列連結は、Gleam言語の基本です。古い言語、たとえばCでは、文字列を連結するのが困難だった。しかし、Gleamでは`string`モジュールの`append`関数のようなツールを使って簡単にできます。選択肢として、`+`演算子を使わないのは、型安全とパフォーマンスを重視するGleamの設計思想に合っています。`append`は内部的に最適化された方法で文字列を結合し、エラーや型の問題を避けます。

## See Also (参照)

- 文字列についてのより広いコンテキスト: [Unicode String](https://unicode.org/reports/tr15/)
- 効率的な文字列操作に関する考察: ["String Concatenation Performance Tips"](https://www.informit.com/articles/article.aspx?p=175137)