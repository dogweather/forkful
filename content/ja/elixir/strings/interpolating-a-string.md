---
date: 2024-01-20 17:50:30.712730-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.006535-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Elixir\u306B\u304A\u3051\u308B\u6587\u5B57\u5217\u88DC\
  \u9593\u306F\u3001Ruby\u8A00\u8A9E\u304B\u3089\u89E6\u767A\u3055\u308C\u307E\u3057\
  \u305F\u3002`#{}`\u3092\u4F7F\u3046\u3053\u3068\u3067\u3001Elixir\u306E\u4EFB\u610F\
  \u306E\u5F0F\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u633F\u5165\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u88DC\u9593\u306F\u30B3\u30F3\u30D1\u30A4\
  \u30EB\u6642\u306B\u5B9F\u884C\u3055\u308C\u307E\u3059\u3002\u3053\u308C\u306F\u3001\
  \u6587\u5B57\u5217\u3092\u76F4\u63A5\u7D50\u5408\u3059\u308B`<>\"string\"`\u3088\
  \u308A\u3082\u52B9\u7387\u7684\u3067\u3059\u3002\u30D0\u30A4\u30CA\u30EA\u9023\u7D50\
  \u3068\u3057\u3066\u3082\u77E5\u3089\u308C\u308B\u3053\u306E\u30C6\u30AF\u30CB\u30C3\
  \u30AF\u306F\u3001Elixir\u306E\u6587\u5B57\u5217\u306E\u4E0D\u5909\u6027(immutability)\u3092\
  \u5229\u7528\u3057\u3066\u9AD8\u901F\u306B\u52D5\u4F5C\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to: (やり方)
```elixir
name = "桜"
greeting = "こんにちは、#{name}さん！"

IO.puts greeting
```
出力：
```
こんにちは、桜さん！
```
```elixir
score = 42
message = "得点は#{score}です。合格です！"

IO.puts message
```
出力：
```
得点は42です。合格です！
```

## Deep Dive (掘り下げ)
Elixirにおける文字列補間は、Ruby言語から触発されました。`#{}`を使うことで、Elixirの任意の式を文字列の中に挿入することができます。補間はコンパイル時に実行されます。これは、文字列を直接結合する`<>"string"`よりも効率的です。バイナリ連結としても知られるこのテクニックは、Elixirの文字列の不変性(immutability)を利用して高速に動作します。

## See Also (関連情報)
- Elixir公式ドキュメント: [String Interpolation](https://hexdocs.pm/elixir/String.html)
- Programming Elixir（プログラミングElixir）: [String Operations](https://pragprog.com/book/elixir/programming-elixir)
