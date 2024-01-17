---
title:                "提取子字符串"
html_title:           "Elixir: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是字符串提取？

字符串提取是指从一个字符串中提取出一部分内容。程序员通常会这样做，因为它可以帮助他们对文本进行处理和分析。

## 怎么做？

在Elixir中，我们可以使用`String.slice/3`函数来提取字符串的子串。例如，我们有一个字符串`"Hello World"`，我们想要提取出`"Hello"`这个子串，我们可以像这样写代码：

```
Elixir String.slice("Hello World", 0, 5)
```

这会返回一个新的字符串`"Hello"`作为结果。这个函数接收三个参数：原始字符串、要提取的子串起始位置和子串的长度。这意味着我们可以从任意位置提取子串，并指定子串的长度。

## 深入讨论

字符串提取的历史可以追溯到早期的编程语言，如C和Java。它们也提供了相似的函数来提取子串。然而，Elixir中的`String.slice/3`函数更加灵活，因为它允许我们指定子串的长度，而不是只能指定子串的结束位置。此外，我们还可以使用Elixir的模式匹配功能来提取子串，这也是一种常用的方法。

除了`String.slice/3`函数，我们还可以使用其他字符串函数来实现字符串提取功能。例如，`String.split/2`函数可以将字符串根据指定的分隔符拆分为子串，然后我们可以从拆分后的列表中选择我们想要的子串。此外，我们还可以使用正则表达式来提取特定模式的子串。

## 参考资料

- [Elixir官方文档：字符串模块](https://hexdocs.pm/elixir/String.html)
- [字符串提取函数的历史背景（英文）](https://en.wikipedia.org/wiki/Substring#History)
- [使用模式匹配来提取子串（英文）](https://blog.red-badger.com/blog/2017/02/06/elixir-pattern-matching-and-strings)