---
title:                "Elixir: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要把字符串转为小写

字符串是编程中常用的一种数据类型，它可以存储文本信息。有时候，我们需要把字符串中的字母全部转为小写，这样方便我们对字符串进行比较或者处理。比如在判断用户输入的用户名是否正确时，就需要将输入的字符串转为小写再与正确的用户名进行比较。

## 如何实现字符串转小写

首先，我们需要使用Elixir中内置的String模块来处理字符串。其中，有一个函数`String.downcase/1`可以将字符串中的所有字母转为小写。下面是一个简单的例子：

```Elixir
name = "Elixir"
lowercase_name = String.downcase(name)
IO.puts(lowercase_name)
```

输出结果为`elixir`。

我们也可以使用`String.downcase/2`函数来指定需要转换的字母表，比如我们想要将希腊字母转为小写：

```Elixir
greek_name = "ΕΛΙΞΙΡ"
lowercase_greek_name = String.downcase(greek_name, "Greek.Uncase")
IO.puts(lowercase_greek_name)
```

输出结果为`ελιξιρ`。

## 深入了解字符串转小写

在Elixir中，字符串是不可变的，也就是说我们无法直接修改某个字符串的内容。当我们使用`String.downcase/1`函数时，它会返回一个新的字符串而不是修改原来的字符串。这种特性叫做“尾部的可连接性”，它可以避免我们在处理较长的字符串时带来的性能问题。

另外，需要注意的是，`String.downcase/2`函数只对ASCII字符有效，如果想要转换其他字符集的字符串，我们可以使用`String.downcase/3`函数来指定字符集。

## 查看更多

- [Elixir String文档](https://hexdocs.pm/elixir/String.html)
- [尾部的可连接性](https://hexdocs.pm/elixir/String.html#string-concatenation-and-appending)
- [不可变性和字符串处理性能](https://elixirforum.com/t/immutability-and-string-manipulation-performance/869)