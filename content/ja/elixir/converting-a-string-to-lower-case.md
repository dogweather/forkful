---
title:    "Elixir: 文を小文字に変換する"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

『なぜ？』

Elixirで文字列を小文字に変換する理由は、プログラム内で文字列を比較したり、正規化したりする必要があるからです。これにより、文字列をより簡単に操作することができ、結果としてコードの品質が向上します。

『方法』

文字列を小文字に変換するには、Elixirの「String.downcase/1」関数を使用します。これは、与えられた文字列をすべて小文字に変換した新しい文字列を返します。以下の例をご覧ください。

```Elixir
string = "Hello World"
downcased_string = String.downcase(string)
IO.inspect(downcased_string)

# => "hello world"
```

このように、変換された文字列が出力されます。また、文字列が空である場合は、空の文字列が返されます。

```Elixir
empty_string = ""
downcased_string = String.downcase(empty_string)
IO.inspect(downcased_string)

# => ""
```

『深堀り』

文字列を小文字に変換するには、実際にはどのような処理が行われるのでしょうか？実は、Elixirでは、Unicodeの正規化が行われた上で、英字を小文字に変換しています。これにより、言語や環境によって異なる文字を含む文字列でも、一貫した処理が行われます。

また、Elixirの文字列はすべてimmutable（不変）なので、小文字に変換した後に再び元の文字列に戻すことはできません。このように、Elixirでは安全性が高いプログラミングが促されます。

『参考』

- String.downcase/1 documentation: https://hexdocs.pm/elixir/String.html#downcase/1
- Unicode normalization: https://unicode.org/faq/normalization.html