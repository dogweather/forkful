---
title:                "Elixir: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーに書き込むことについてエルクサーのプログラミングブログをお読みいただきありがとうございます。慣れないうちは、標準出力に書き込むことが一般的ですが、標準エラーへの書き込みは、デバッグ作業やエラーの表示など、プログラムの機能度を向上させるために重要な役割を果たします。

## 使い方

```elixir
IO.puts("Standard output message")
```

標準出力に書き込むには、上記のように`IO.puts`関数を使用します。しかし、標準エラーに書き込む場合は、`IO.puts/1`関数を`IO.stderr`に渡します。

```elixir
IO.puts(IO.stderr, "Standard error message")
```

これにより、標準エラーへのメッセージが出力されます。また、`IO.inspect/2`関数を使用することで、標準エラーに変数の内容を表示させることもできます。

```elixir
num = 10
IO.inspect(IO.stderr, num) # => 標準エラーに 10というメッセージが出力される
```

## 深堀り

標準エラーへの書き込みは、主にエラーハンドリングやデバッグ作業で使用されます。プログラムで発生したエラーを即時に表示させることで、解決されるべき問題箇所を特定することができます。

また、`IO.inspect/2`関数による標準エラーへの出力は、プログラムの実行速度にほとんど影響を与えません。これは、エラーが発生したときにのみ出力されるためです。

## 併せて読みたい

- [Elixir公式ドキュメント - IOモジュール](https://hexdocs.pm/elixir/IO.html)
- [Elixir公式ドキュメント - Kernelモジュール](https://hexdocs.pm/elixir/Kernel.html#puts/1)
- [Elixir School - IOモジュール](https://elixirschool.com/ja/lessons/basics/io/)