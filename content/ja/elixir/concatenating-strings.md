---
title:                "文字列の結合"
html_title:           "Elixir: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結をすることの利点について知りたいと思っている方もいるかもしれません。私たちは、Elixir における文字列の連結の重要性を説明するためにこの記事を書きました。

## 連結する方法

Elixir では、文字列を簡単に連結することができます。以下のコード例を見てください。

```Elixir
string1 = "Hello"
string2 = "world"

final_string = string1 <> " " <> string2

IO.puts(final_string)
```

このコードを実行すると、"Hello world"という文字列が出力されます。ここでは、`<>` 演算子を使用して、`string1` と `string2` を連結し、間にスペースを追加しています。

また、複数の文字列を連結する際には、`Enum.join/2` 関数を使用することもできます。以下のコードを見てください。

```Elixir
strings = ["Hello", " ", "world"]
final_string = Enum.join(strings)

IO.puts(final_string)
```

このコードも同じ結果が得られますが、`Enum.join/2` 関数を使用することでより柔軟に文字列を連結することができます。

## 深く掘り下げる

Elixir では、文字列を連結する際にメモリのアロケーションが起こらないように最適化されています。そのため、大量の文字列を連結してもパフォーマンスに影響が出にくいのです。

また、`<>` 演算子は、リストを連結する際にも使用することができます。しかし、ここでは文字列の連結に焦点を当てて説明しています。

## See Also

- [Elixir ドキュメント：文字列連結](https://hexdocs.pm/elixir/String.html#concatenation-and-interpolation)
- [Elixir School：文字列操作](https://elixirschool.com/jp/lessons/basics/string-concatenation/)