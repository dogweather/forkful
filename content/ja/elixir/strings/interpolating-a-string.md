---
date: 2024-01-20 17:50:30.712730-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.594313-06:00'
model: gpt-4-1106-preview
summary: .
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
