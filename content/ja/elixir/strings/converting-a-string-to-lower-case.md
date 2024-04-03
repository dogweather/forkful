---
date: 2024-01-20 17:38:02.826242-07:00
description: "How to: \u3084\u308A\u65B9\uFF1A Elixir\u3067\u306F\u3001`String.downcase/1`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u30B3\u30FC\
  \u30C9\u4F8B\u3068\u51FA\u529B\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.595539-06:00'
model: gpt-4-1106-preview
summary: "\u3084\u308A\u65B9\uFF1A\nElixir\u3067\u306F\u3001`String.downcase/1`\u95A2\
  \u6570\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u30B3\u30FC\
  \u30C9\u4F8B\u3068\u51FA\u529B\u3092\u793A\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
