---
date: 2024-01-20 17:47:24.150339-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) \u5728Elixir\u4E2D\uFF0C\u4F7F\u7528\
  `String.length/1`\u51FD\u6570\u53EF\u4EE5\u83B7\u5F97\u5B57\u7B26\u4E32\u7684\u957F\
  \u5EA6\u3002\u793A\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.690549-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u505A\uFF1A) \u5728Elixir\u4E2D\uFF0C\u4F7F\u7528`String.length/1`\u51FD\
  \u6570\u53EF\u4EE5\u83B7\u5F97\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\u3002\u793A\u4F8B\
  \uFF1A."
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (如何做：)
在Elixir中，使用`String.length/1`函数可以获得字符串的长度。示例：

```elixir
string = "你好，世界！"
length = String.length(string)
IO.puts(length)
```

输出:

```
6
```

## Deep Dive (深入探索)
历史上，字符串长度的概念因编程语言和文本编码的差异而复杂。在 Elixir 中，`String.length/1` 返回的是字符串中的 Unicode 字符数量，也叫做“graphemes”。比如，emoji 或者带重音的字符都算单一字符。

Elixir 使用 UTF-8 编码, 所以 `String.length/1` 考虑了多字节字符。不过，如果你想知道字节长度，可以使用 `byte_size/1`。

```elixir
string = "你好，世界！"
byte_size = byte_size(string)
IO.puts(byte_size)
```

输出:

```
15
```

`String.length/1` 不同于 `Kernel.length/1`，后者用于确定集合（如列表或元组）的长度。

其他语言可能导入外部库来处理字符串，但 Elixir 的标准库就已内置了这个功能，体现了它对文本处理的高度重视。

## See Also (另请参阅)
- Elixir 的官方文档关于字符串处理的内容: [Elixir String Docs](https://hexdocs.pm/elixir/String.html)
- Unicode 标准说明了如何处理多种语言的字符: [Unicode Standard](http://www.unicode.org/standard/standard.html)
- Elixir 论坛，有关字符串处理的讨论: [Elixir Forum](https://elixirforum.com/)
- 了解 UTF-8 和字符编码的细节: [UTF-8 Wikipedia](https://en.wikipedia.org/wiki/UTF-8)
