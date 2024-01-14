---
title:                "Elixir: 将字符串转换为小写"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

Elixir是一种功能强大的编程语言，它具有简洁的语法和高效的性能。在Elixir中，字符串操作是一项非常常见的任务，其中有时需要将字符串转换为小写。这样做的原因可能是为了统一格式，或者为了方便后续的字符串匹配操作。无论是什么原因，掌握如何将字符串转换为小写是一项非常有用的技能，有助于提高编码效率。

## 如何将字符串转换为小写

在Elixir中，可以使用 `String.downcase/1` 函数将字符串转换为小写。这个函数接受一个字符串作为参数，并返回一个新的小写的字符串。下面是一个示例代码和输出：

```Elixir
iex> String.downcase("HELLO WORLD")
"hello world"
```

可以看到，原来全大写的字符串已经被成功转换为小写。

## 深入了解字符串小写转换

在Elixir中，字符串是不可变的，这意味着任何对字符串的操作都会返回一个新的字符串，而不会改变原来的字符串。而 `String.downcase/1` 函数也不例外。它通过利用 `String.to_charlist/1` 函数将字符串转换为字符列表，然后使用 `Enum.map/2` 函数对每个字符进行小写转换，最后使用 `List.to_string/1` 函数将字符列表转换回字符串。这个过程也可以用下面的代码来表示：

```Elixir
String.downcase("Hello") == "Hello" |> String.to_charlist() |> Enum.map(&String.downcase/1) |> List.to_string()
```

值得一提的是，Elixir中的字符串操作采用Unicode标准，因此可以正确地将任何语言的字符串转换为小写。

## 查看更多

- [Elixir字符串操作文档](https://hexdocs.pm/elixir/String.html)
- [Elixir教程](https://elixir-lang.org/getting-started/introduction.html) 
- [Unicode标准网站](https://www.unicode.org/)

## 参考资料

- [Elixir Docs: String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html)
- [Unicode Official Website](https://www.unicode.org/)