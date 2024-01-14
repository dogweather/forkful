---
title:    "Elixir: 将字符串转换为小写"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么

有时候，在编程过程中，我们需要将字符串转换成小写形式。这可能是为了与其他字符串进行比较，或者将字符串作为URL的一部分。使用Elixir，我们可以很容易地将字符串转换成小写形式。

## 如何使用

我们可以使用`String.downcase/1`函数来将字符串转换成小写形式。下面是一个简单的例子：

```Elixir
iex> String.downcase("HELLO WORLD")
"hello world"
```

我们也可以将多个字符串一起转换：

```Elixir
iex> String.downcase("HELLO", "WORLD")
["hello", "world"]
```

另外，我们也可以使用`String.downcase!/1`函数，在原始字符串上直接进行转换，而不是返回一个新的字符串。

```Elixir
iex> str = "HELLO WORLD"
"HELLO WORLD"
iex> String.downcase!(str)
"hello world"
iex> str
"hello world"
```

## 深入了解

在Elixir中，将字符串转换成小写形式实际上是相当简单的。这是因为每个字符都有一个对应的小写形式，因此Elixir只需要检查每个字符，并将其转换成小写形式即可。

另外值得一提的是，`String.downcase/1`函数是基于Unicode标准的，因此它可以正确地处理各种语言中的特殊字符。

## 参考链接
- [Elixir官方文档-String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Elixir官方文档-String.downcase!/1](https://hexdocs.pm/elixir/String.html#downcase!/1)
- [Elixir字符串转换的性能对比](https://medium.com/@brambulate/elixir-string-transformation-performance-comparison-32d4e08cf5b0)

## 参看

如果你对Elixir中的字符串操作感兴趣，可以继续阅读以下文章：
- [Elixir中的字符串操作指南](https://elixir-lang.org/getting-started/string.html)
- [Elixir中常用的字符串操作函数](https://www.jessejanderson.com/elixir-string-functions/)
- [使用Elixir构建字符串处理应用程序](https://hackernoon.com/building-a-string-processing-application-in-elixir-cb2d7ef036ea)