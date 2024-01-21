---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:02.826242-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？
文字列を小文字に変換する、つまり「ケース変換」とは文字列中の大文字をすべて小文字にすることです。検索、ソート、ユーザー入力の正規化など、一貫性と比較のためにプログラマはこれを行います。

## How to:
やり方：
Elixirでは、`String.downcase/1`関数を使って簡単に文字列を小文字に変換できます。以下にコード例と出力を示します。

```elixir
string = "HELLO, World!"
IO.puts String.downcase(string)
# 出力: hello, world!
```

## Deep Dive
深掘り：
Elixirの`String.downcase/1`はUnicodeをサポートしており、多言語のテキストで正確なケース変換が可能です。歴史的には、ASCIIだけをサポートした関数が使われていましたが、国際化のニーズが高まるにつれて、より包括的な対応が求められるようになりました。`downcase`の代わりに正規表現や自前のロジックを使う方法もありますが、パフォーマンスと信頼性の観点から、組み込み関数の使用が推奨されます。

## See Also
関連情報：
- Elixirの公式ドキュメント: [String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- Unicodeのケースマッピングに関する情報: [Unicode Case Mappings](https://www.unicode.org/reports/tr21/)
- Elixir School: [Strings](https://elixirschool.com/en/lessons/basics/strings/)