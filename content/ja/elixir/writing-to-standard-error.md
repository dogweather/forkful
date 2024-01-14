---
title:    "Elixir: 標準エラーに書き込みする"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Elixir で標準エラー出力を書くために始めよう

## なぜ

プログラミングをしていると、時にエラーが発生します。その際には、デバッグ用の出力を行うことが重要です。標準エラー出力を行うことで、エラーの原因を特定し、修正することができます。

## 方法

Elixirでは、`IO.write`を使用して文字列を標準エラー出力することができます。以下のコードは、標準エラー出力に"Hello World!"を表示する例です。

```Elixir
IO.write(:stderr, "Hello World!")
```

実行結果は以下のようになります。

```
Hello World!
```

また、変数を標準エラー出力することもできます。以下の例では、変数`number`の値を出力しています。

```Elixir
number = 10
IO.write(:stderr, "The number is #{number}")
```

実行結果は以下のようになります。

```
The number is 10
```

## ディープダイブ

標準エラー出力を行う際には、エラーの種類に応じて異なる方法を使用することができます。

例えば、`IO.inspect/2`を使用することで、標準エラー出力にデバッグ用の情報を出力することができます。

```Elixir
IO.inspect(some_variable, label: "Debug Info")
```

また、標準エラー出力の内容をファイルに出力することもできます。`File.write/3`を使用して、標準エラー出力をファイルに書き込むことができます。

```Elixir
File.write("error.log", "Error message")
```

## 以下も見てみる
- [IOモジュールのドキュメント](https://hexdocs.pm/elixir/IO.html)
- [Elixirでデバッグするためのトリック](https://flaviocopes.com/elixir-debugging-tricks/)