---
title:                "获取字符串的长度"
date:                  2024-01-20T17:47:24.150339-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
找出字符串的长度就是确定字符串中字符的数量。程序员经常需要这么做来处理文本数据，验证输入或优化性能。

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
