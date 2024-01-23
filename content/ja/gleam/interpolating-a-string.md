---
title:                "文字列の補間"
date:                  2024-01-20T17:50:45.306835-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なに？どうして？)
文字列補間とは、固定のテキストの中に変数や式の値を埋め込むことです。プログラマは可読性やメンテナンス性を高めるためにこれを行います。

## How to: (やり方)
Gleamでは`String.interpolate`を使います。使い方の例を見てみましょう。

```gleam
import gleam/io

fn main() {
  let name = "Mika"
  let message = String.interpolate("こんにちは、${name}さん!")
  io.print(message)
}
```

出力：
```
こんにちは、Mikaさん!
```

## Deep Dive (掘り下げ)
歴史的には、文字列補間は多くの言語で採用され、異なる構文を使用しています。例えば、Rubyでは`"#{expression}"`、Pythonでは`f"{expression}"`が使われます。Gleamの場合、`${}`構文が使われ、その中に式を入れることができます。

Gleamの内部では、文字列補間は単に文字列の集合として実装され、実行時に連結されます。これにより、パフォーマンスが向上し、コンパイル時に型チェックが行われるため安全性が確保されます。

選択肢としては、文字列の`++`演算子を使って手動で連結する方法もありますが、文字列補間の方が簡潔で読みやすいコードになります。

## See Also (関連情報)
- [Comparative study of string interpolation across different languages](https://en.wikipedia.org/wiki/String_interpolation)
