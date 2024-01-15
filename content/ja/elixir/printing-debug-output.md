---
title:                "デバッグ出力の印刷"
html_title:           "Elixir: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# なぜデバッグ出力を行うのか

デバッグ出力とは、コードを実行する際に、実行中のプログラムの状態や値を確認するために使用される機能です。エラーが発生した際に、その原因を特定するためにも役立ちます。また、プログラムの正確性を保証するためにも重要です。

## 方法

デバッグ出力を行うには、`IO.inspect/2` という関数を使用します。この関数は、任意のデータを受け取り、その内容を出力します。例えば、以下のように使用します。

```Elixir
IO.inspect("Hello world")
```

上記のコードを実行すると、ターミナル上に `"Hello world"` という文字列が出力されます。また、変数や関数の値を出力する際には、以下のように使用します。

```Elixir
name = "John"
IO.inspect(name)

addition = fn x, y -> x + y end
IO.inspect(addition.(3, 4))
```

出力結果は、それぞれ `"John"` と `7` となります。

## 深堀り

デバッグ出力を行う際には、必要に応じて `IO.inspect/2` の第二引数にオプションを指定することができます。例えば、以下のように `pretty: true` を指定すると、出力結果が整形されて見やすくなります。

```Elixir
IO.inspect([1, 2, 3], pretty: true)
```

出力結果は、以下のようになります。

```Elixir
[
  1,
  2,
  3
]
```

また、`IO.inspect/2` の第二引数に渡すオプションの一つに `label: "文字列"` というものがあり、指定した文字列が出力されるようになります。これを利用すると、どの箇所のデバッグ出力かを明示することができます。

## 併せて参照

* [Elixir 公式ドキュメント - IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
* [プログラミング初心者も活用できる Elixir のメリット・デメリットまとめ](https://herp.cafe/articles/eliixr-pros-cons)（日本語）