---
title:                "Elixir: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要操作字符串。有时候，我们需要知道一个字符串的长度。知道一个字符串的长度可以帮助我们正确地处理它，例如，限制用户输入的字符数或者追踪数据中特定字段的长度。在Elixir编程中，我们可以使用内置的函数来轻松地获得字符串的长度。接下来，我们将介绍如何使用Elixir来找到字符串的长度，以及背后的原理。

## 如何做

在Elixir中，我们可以使用String模块中的`length/1`函数来获得字符串的长度。它接受一个字符串作为参数，并返回该字符串的字符数。让我们来看一个例子：

```Elixir
string = "Hello World"
length = String.length(string)
IO.puts(length)
```

输出为:

```
11
```

我们可以看到，`length/1`函数返回了字符串`"Hello World"`的长度11。让我们再来看一个更复杂的例子：

```Elixir
string = "你好，世界"
length = String.length(string)
IO.puts(length)
```

输出为:

```
5
```

即使字符串中包含中文字符，`length/1`函数也能正确地返回长度。这说明Elixir对于不同的编码方式都有良好的支持。除了`length/1`函数，我们还可以使用`byte_size/1`函数来获取字符串的字节数。这在处理非ASCII字符时很有用。

## 深入了解

在Elixir中，字符串是以UTF-8编码的。这意味着，每个字符在内存中占用的字节数不同，取决于它的编码。因此，使用`length/1`函数获得的字符串长度实际上是它包含的字符数，并不完全等于它的字节数。例如，一个中文字符可能占用2-4个字节，但在使用`length/1`函数时，它仍然被计算为一个字符。因此，仅仅依靠字符数来处理字符串的长度可能会导致错误，特别是在涉及多字节字符的情况下。在这种情况下，最好使用`byte_size/1`函数来获得字符串的字节数。

## 参考链接

- [Elixir官方文档 - String模块](https://hexdocs.pm/elixir/String.html)
- [Elixir官方文档 - byte_size/1函数](https://hexdocs.pm/elixir/Kernel.html#byte_size/1)
- [Elixir官方文档 - length/1函数](https://hexdocs.pm/elixir/String.html#length/1)