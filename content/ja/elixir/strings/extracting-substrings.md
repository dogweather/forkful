---
date: 2024-01-20 17:45:32.735831-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u4E00\u90E8\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u3092\u3001\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3068\u8A00\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C7\
  \u30FC\u30BF\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u60C5\u5831\
  \u3092\u53D6\u5F97\u30FB\u8868\u793A\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3088\u304F\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.873262
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u4E00\u90E8\u3092\u53D6\
  \u308A\u51FA\u3059\u3053\u3068\u3092\u3001\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3068\u8A00\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C7\
  \u30FC\u30BF\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u60C5\u5831\
  \u3092\u53D6\u5F97\u30FB\u8868\u793A\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3088\u304F\u884C\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定の一部を取り出すことを、部分文字列を抽出すると言います。これは、データを操作したり、特定の情報を取得・表示したりするためにプログラマーがよく行います。

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
