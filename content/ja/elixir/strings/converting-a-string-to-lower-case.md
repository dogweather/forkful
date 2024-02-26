---
date: 2024-01-20 17:38:02.826242-07:00
description: "\u4F55\u3068\u306A\u305C\uFF1F \u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u3001\u3064\u307E\u308A\u300C\u30B1\u30FC\u30B9\u5909\
  \u63DB\u300D\u3068\u306F\u6587\u5B57\u5217\u4E2D\u306E\u5927\u6587\u5B57\u3092\u3059\
  \u3079\u3066\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u691C\
  \u7D22\u3001\u30BD\u30FC\u30C8\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\
  \u898F\u5316\u306A\u3069\u3001\u4E00\u8CAB\u6027\u3068\u6BD4\u8F03\u306E\u305F\u3081\
  \u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.748001-07:00'
model: gpt-4-1106-preview
summary: "\u4F55\u3068\u306A\u305C\uFF1F \u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u3001\u3064\u307E\u308A\u300C\u30B1\u30FC\u30B9\u5909\
  \u63DB\u300D\u3068\u306F\u6587\u5B57\u5217\u4E2D\u306E\u5927\u6587\u5B57\u3092\u3059\
  \u3079\u3066\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u691C\
  \u7D22\u3001\u30BD\u30FC\u30C8\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\
  \u898F\u5316\u306A\u3069\u3001\u4E00\u8CAB\u6027\u3068\u6BD4\u8F03\u306E\u305F\u3081\
  \u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
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
