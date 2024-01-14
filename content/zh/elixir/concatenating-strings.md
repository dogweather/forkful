---
title:    "Elixir: 连接字符串"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要在编程中将多个文本片段组合起来时，字符串连接(concatenation)就成为了必不可少的工具。通过将多个字符串连接在一起，我们可以创建出更复杂的文本内容，这在处理用户输入或输出时尤为重要。

## 如何

```Elixir
"Hello" <> " " <> "World!"
```
将会输出："Hello World!"

可以使用 `<>` 运算符来连接两个字符串，并创建一个新的字符串。需要注意的是，连接的内容必须是字符串，否则会报错。

```Elixir
"1" <> "2" <> 3
```

将会报错，因为 3 是一个整数，而不是一个字符串。

我们也可以使用 `Enum.into/2` 函数来将多个字符串连接成一个字符串。这个函数可以将一个字符串列表转换为一个字符串，并可以指定一个分隔符。

```Elixir
Enum.into(["I", "like", "to", "code"], " ") 
```

将会输出："I like to code"

## 深入探讨

Elixir 中的字符串连接实际上是通过元组拼接来实现的。每次使用 `<>` 运算符连接字符串时，都会创建一个元组，其中包含了之前的字符串和新的字符串。然后通过 `IO.iodata_to_binary/1` 函数来将这个元组转换为一个二进制字符串。

这种实现方式的好处是，每次字符串连接都不需要重新分配内存，可以避免字符串的不断复制，从而提高了性能。

## 参考链接

- [Elixir字符串连接文档](https://hexdocs.pm/elixir/String.html#module-concatenation)
- [Elixir核心函数Enum.into/2文档](https://hexdocs.pm/elixir/Enum.html#into/2)
- [深入了解Elixir中字符串的运行机制](https://blog.danielberkompas.com/elixir-low-level-string-concatenation.html)

## 参见