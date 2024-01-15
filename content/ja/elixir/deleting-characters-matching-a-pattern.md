---
title:                "パターンに一致する文字を削除する"
html_title:           "Elixir: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

削除する文字列に一致する文字を削除することの利点は何でしょうか？それは、文字の一致がより正確に削除されることを意味します。

## How To

  ```
  Elixir.replace("Hello World", ~r/W/, "")
  #=> "Hello orld"
  Elixir.replace("Nice to meet you", ~r/.e/, "")
  #=> "Nicto ot u"
  Elixir.replace("Welcome to Elixir", ~r/e+i/, "")
  #=> "Wlcomtoxir"
  ```

上記のコードは、`replace`関数を使用して、指定された文字列内で一致するすべての文字を削除する方法を示しています。`~r/`と`/`の間に削除したい文字列の正規表現パターンを記述します。最後の引数に空白の文字列を渡すことで、一致する文字が削除されるように指示します。`~r/`と`/`を使用することで、正規表現を表すことができます。

## Deep Dive

正規表現を使用することで、より高度な文字列の操作を行うことができます。例えば、正規表現で削除したい文字列のパターンを指定することで、より柔軟に削除することができます。さらに、正規表現を学ぶことで、他のパターンマッチングの機能や文字列操作関数を利用することができるようになります。

## See Also

- Elixir's String module: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regular Expressions in Elixir: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)