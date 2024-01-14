---
title:    "Elixir: 查找字符串的长度"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 为什么：

在编程中，我们经常需要操作字符串，而字符串的长度是一个基本的信息。了解字符串的长度可以帮助我们更高效地处理字符串，比如截取子串等操作。

## 怎样实现：

在Elixir中，可以使用内置函数`String.length/1`来获取字符串的长度。下面是一个示例代码：

```elixir
str = "你好，世界！"
IO.puts String.length(str)
```

运行结果为：

```
7
```

## 深入了解：

在Elixir中，字符串的长度实际上是指字符的数量，而不是字节数。这是因为Elixir使用UTF-8编码来表示字符串，而UTF-8中每个字符的长度是可变的。因此，即使一个中文字符的字节数为3，但在Elixir中仍被视为一个字符。

另外，如果想要更准确地获取字符串的字节数，可以使用内置模块`:unicode`的函数`Unicode.length/1`。

# 参考链接：

- [Elixir官方文档-String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir官方文档-Unicode.length/1](https://hexdocs.pm/elixir/unicode-utf8.html#length/1)
- [UTF-8编码规则](https://www.fileformat.info/info/unicode/utf8.htm)