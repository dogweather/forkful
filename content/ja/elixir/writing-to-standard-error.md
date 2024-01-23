---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラーへの書き込みは、エラーメッセージや警告を標準出力とは別に出力することです。プログラマーは、出力の整理やデバッグ情報の分離にこれを使います。

## How to: (方法)
```elixir
# 標準エラーにメッセージを出力
IO.puts(:stderr, "エラーが発生しました")

# サンプル出力: コンソールに直接表示
```
標準エラーへの出力はこれだけです。シンプル。

## Deep Dive (深掘り)
標準エラーの概念は、UNIXの初期より存在します。ElixirだとIOモジュールが担当。`:stderr`アトムを使う方法の他にも、`:stdio`エラータプルや低レベルのErlang関数（`:erlang.display/1`など）があります。実装はErlangの機能に依存しています。

## See Also (参照)
- [Elixir IO Module](https://hexdocs.pm/elixir/IO.html)
- [Erlang :erlang Module](http://erlang.org/doc/man/erlang.html)
- [UNIX Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
