---
title:                "Elixir: 「標準エラーに書き込む」"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをするとき、時々私たちはエラーメッセージを受け取ります。しかし、それらのメッセージがどこから来たのか疑問に思ったことはありませんか？それは標準エラー出力（Standard Error）です。今回は、Elixirで標準エラー出力をどのように書くか、そしてなぜそれが重要なのかについてお話しします。

## どのように書くか

```elixir
IO.puts("このメッセージは標準出力に表示されます")
IO.puts("このメッセージは標準エラー出力に表示されます", [:stderr])
```

上記のように、`IO.puts`関数を使用してメッセージを出力します。`[:stderr]`パラメータを渡すことで、メッセージを標準エラー出力に送ることができます。また、`IO.puts/2`のように、第二引数に`stderr`を明示的に渡すこともできます。

もしも出力先を指定しない場合、デフォルトでは標準出力にメッセージが表示されます。

## 深堀り

なぜ私たちは標準エラー出力を使用するのでしょうか？その理由は、プログラムが何らかのエラーを検出した際、そのメッセージを確実にコンソールに表示するためです。標準出力は、実行結果や他の情報で埋め尽くされる可能性があり、重要なエラーメッセージを見逃してしまうことがあります。

また、標準エラー出力は、エラーログを保存するために使用することもできます。例えば、`File.write`関数を使用して、エラーメッセージをテキストファイルに保存することができます。

## 参考リンク

- [Elixir公式ドキュメント - IO.puts/1](https://hexdocs.pm/elixir/IO.html#puts/1)
- [Elixir公式ドキュメント - IO.puts/2](https://hexdocs.pm/elixir/IO.html#puts/2)
- [The Power of Elixir’s standard_errors](https://hackernoon.com/the-power-of-elixirs-standard-errors-59c180f3ad60)