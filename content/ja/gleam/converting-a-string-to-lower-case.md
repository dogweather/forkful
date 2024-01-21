---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:22.613780-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するのは、大文字と小文字を区別しない比較をするためです。データの正規化や検索などで利用されます。

## How to: (やり方)
```gleam
import gleam/string

pub fn main() {
  let greeting = "Hello, World!"
  let lower_greeting = string.to_lower(greeting)
  io.println(lower_greeting) // "hello, world!"
}
```

## Deep Dive (深い潜水)
文字列を小文字に変換する操作は多くのプログラミング言語に備わっています。英文字だけではなく、国際化を意識したUnicode文字にも対応することが重要です。Gleamでは`string.to_lower`を使って実現できますが、これは内部的にはRustの`.to_lowercase()`を利用しています。またこの操作の別の方法としては、ASCII限定で自分で関数を作成することもできますが、実用的ではないことが多いです。

## See Also (関連項目)
- Unicode case mapping FAQs: [http://unicode.org/faq/casemap_charprop.html](http://unicode.org/faq/casemap_charprop.html)