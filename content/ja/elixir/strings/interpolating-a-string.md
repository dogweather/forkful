---
title:                "文字列の補間"
aliases:
- /ja/elixir/interpolating-a-string.md
date:                  2024-01-20T17:50:30.712730-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/interpolating-a-string.md"
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
