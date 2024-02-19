---
aliases:
- /ja/elixir/interpolating-a-string/
date: 2024-01-20 17:50:30.712730-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u3084\u5F0F\
  \u306E\u5024\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u306A\u305C\u4F7F\u3046\u304B\u3068\u3044\u3046\u3068\u3001\
  \u52D5\u7684\u306A\u5185\u5BB9\u3092\u6271\u3044\u3084\u3059\u304F\u3059\u308B\u305F\
  \u3081\u3001\u30B3\u30FC\u30C9\u3092\u304D\u308C\u3044\u306B\u4FDD\u3064\u305F\u3081\
  \u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.630098
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u3084\u5F0F\
  \u306E\u5024\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u306A\u305C\u4F7F\u3046\u304B\u3068\u3044\u3046\u3068\u3001\
  \u52D5\u7684\u306A\u5185\u5BB9\u3092\u6271\u3044\u3084\u3059\u304F\u3059\u308B\u305F\
  \u3081\u3001\u30B3\u30FC\u30C9\u3092\u304D\u308C\u3044\u306B\u4FDD\u3064\u305F\u3081\
  \u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは、変数や式の値を文字列の中に埋め込むことです。なぜ使うかというと、動的な内容を扱いやすくするため、コードをきれいに保つためです。

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
