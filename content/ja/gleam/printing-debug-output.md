---
title:                "デバッグ出力の印刷"
html_title:           "Gleam: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

##なぜ？

デバッグ出力を行うことがどのように役立つのかを理解するためには、まずはなぜそれが必要なのかを知る必要があります。デバッグ出力は、コード内の不具合を特定する際に重要なツールです。デバッグ出力を使用することで、コードの実行中にどのような値が代入されているのかを確認し、問題をより早く発見することができます。

##やり方

デバッグ出力を行うためには、Gleamの"```log```"関数を使用します。この関数は、引数として任意の型のデータを受け取り、それを標準出力に出力します。例えば、文字列をデバッグ出力するには以下のように記述します。

```
Gleam.log("Hello, world!")
```

もし変数に割り当てられた値を出力したい場合は、その変数を"```{}```"内に記述します。

```
let name = "John"
Gleam.log("Hello, {}", name)
```

さらに、複数の値を出力したい場合は、"```{}```"をそれぞれの値の位置に対応させます。

```
let name = "John"
let age = 25
Gleam.log("My name is {} and I am {} years old.", name, age)
```

上記のコードの実行結果は、以下のようになります。

```
My name is John and I am 25 years old.
```

##深堀り

デバッグ出力には、さまざまなオプションがあります。例えば、"```log```"関数の第二引数に"```format_opts```"を指定することで、出力する値のフォーマットを指定することができます。また、"```log```"関数を使用する際には、必ず先頭に"```import gleam/io```"を記述するように注意しましょう。

さらに、Gleamにはデバッグ出力を行うための専用モジュールとして"```gleam/debug```"があり、さまざまな便利な関数を提供しています。このモジュールを使用することで、より効率的かつ柔軟なデバッグ出力を行うことができます。

##参考リンク

- [Gleamドキュメント - ロギング](https://gleam.run/book/tour/logging)
- [Gleamドキュメント - デバッグ](https://gleam.run/book/how_to/debugging)
- [Gleamドキュメント - 標準ライブラリ](https://gleam.run/lib/standard-library)