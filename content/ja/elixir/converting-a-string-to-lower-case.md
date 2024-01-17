---
title:                "文字列を小文字に変換する"
html_title:           "Elixir: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何でやるの? 

文字列を小文字に変換するとは、文字列のすべての大文字を小文字に変換することです。プログラマーがこれを行うのは、文字列を比較や検索する際に大文字と小文字を区別しないようにするためです。

## 方法:

```Elixir
"STRING" |> String.downcase() #=> "string"
"Hello World" |> String.downcase() #=> "hello world"
```

## 深く掘り下げる:

文字列を小文字に変換するアイデアは、コンピューターの言語によって異なります。例えば、Pythonでは `lower()` メソッドを使用していますが、Elixirでは `downcase()` 関数を使用します。また、文字列を小文字にすることにより、大文字と小文字を区別しない検索や比較が簡単になり、より柔軟なプログラミングが可能になります。

## 関連情報を見る:

- [Elixir Documentation - String module](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Python Documentation - lower() method](https://docs.python.org/3/library/stdtypes.html#str.lower)