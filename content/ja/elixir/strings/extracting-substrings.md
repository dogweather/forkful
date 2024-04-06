---
date: 2024-01-20 17:45:32.735831-07:00
description: "How to: (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306E\u51FA\
  \u529B\u306F `\u30A8\u30EA\u30AF\u30B5\u30FC` \u306B\u306A\u308B\u3067\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.544569-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306E\u51FA\u529B\u306F\
  \ `\u30A8\u30EA\u30AF\u30B5\u30FC` \u306B\u306A\u308B\u3067\u3057\u3087\u3046\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (やり方)
```Elixir
# 文字列を定義する
original_string = "こんにちは、エリクサーの世界！"

# 部分文字列を取り出す
substring = String.slice(original_string, 6, 5)

# 結果を表示する
IO.puts(substring)
```
このコードの出力は `エリクサー` になるでしょう。

## Deep Dive (深く掘り下げる)
Elixirでは`String`モジュールを使って文字列を操作します。`String.slice/3`はElixirの初期バージョンから使われてきました。他の言語にも同様の機能がありますが、Elixir特有の並行処理や不変性の文脈で利用されます。また、Elixirの文字列はバイナリとして表現され、UTF-8エンコーディングで扱われるため、バイトサイズが文字数と一致しないことがあります。それを考慮して、`String.slice/3`は文字の境界を正しく扱えるようになっています。

## See Also (関連情報)
- Elixir公式ドキュメント: [String module](https://hexdocs.pm/elixir/String.html)
- Learn Elixir: [文字列とバイナリ](https://elixirschool.com/ja/lessons/basics/strings/)
