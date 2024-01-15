---
title:                "文字列の最初を大文字にする"
html_title:           "Elixir: 文字列の最初を大文字にする"
simple_title:         "文字列の最初を大文字にする"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換する行為は、プログラムの中で文字列を一貫性を持って扱いたいときに便利です。

## 方法
```elixir
string = "hello world"
String.capitalize(string) # => "Hello world"
```

文字列を大文字に変換するには、Elixirの`String.capitalize/1`関数を使用します。この関数は、与えられた文字列の先頭の文字を大文字に変換します。上記の例では、文字列 "hello world" が "Hello world" に変換されます。

## ディープダイブ
`String.capitalize/1`関数は、単に最初の文字を大文字に変換するだけではありません。例えば、`String.capitalize/1`関数はアクセント記号付きの文字、全角文字、およびアルファベット以外の文字にも対応しています。また、英字以外の言語にも対応しているため、多言語対応のプログラムにも利用することができます。

さらに、`String.capitalize/1`関数の代わりに`String.capitalize/2`関数を使用すると、指定したロケールに基づいて文字列を大文字に変換することができます。これにより、特定の言語や地域に合わせた大文字変換を行うことが可能になります。

## 他に見る

- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir School: Strings](https://elixirschool.com/ja/lessons/basics/binary-and-strings/#strings)