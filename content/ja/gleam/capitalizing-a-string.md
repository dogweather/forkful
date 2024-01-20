---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
文字列を大文字化するとは、文字列内の各単語の最初の文字を大文字に変換することです。これは、タイトル、見出し、または固有名詞を表記する際によく行われます。

## How to: (やり方)
Gleamでは文字列の大文字化は`String.to_uppercase`関数を使いましょう。例と出力は以下の通りです。

```gleam
import gleam/string

pub fn main() {
  let greeting = "hello world"
  let shout = string.to_uppercase(greeting)
  shout
}
```

サンプル出力:

```
"HELLO WORLD"
```

## Deep Dive (詳細な情報)
歴史的には、大文字使用は文献で重要な単語を際立たせるためです。Gleamの`String.to_uppercase`は、さまざまな文字エンコーディングと言語に対応するUnicodeの大文字変換を採用しています。これはUTF-8によく最適化されているため、多くの言語で効率的に使えます。代替手段として、文字列の特定の部分だけを大文字にすることも可能ですが、その際には追加の関数や手作業が必要です。実装の詳細では、性能と互換性のバランスが重要になります。

## See Also (関連情報)
- Unicodeについての詳細: [Unicode.org](http://unicode.org/)