---
title:    "Elixir: 寻找字符串的长度"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么

字符串的长度是一个常见的编程问题，因为它涉及到对字符串操作的基本理解和技能。它是诸如验证用户输入以及数据处理等任务中几乎必不可少的一步。在Elixir编程中，掌握如何找到字符串的长度是非常重要的，因此让我们来学习一下如何做到这一点。

# 如何

```Elixir
string = "Hello World"
IO.puts String.length(string)
```

这个简单的代码块可以输出：`11`，因为“Hello World”这个字符串总共有11个字符。让我们通过另一个例子来进一步理解这个概念。

```Elixir
string = "你好世界"
IO.puts String.length(string)
```

这次，我们得到的输出将是`5`，因为这个字符串由一个中文字符和四个英文字符组成，总共是5个字符。这说明，Elixir中的长度计算并不是简单地基于字符的个数，而是基于每个字符的编码长度。这对于处理各种不同编码格式的字符串非常有用。

# 深入探讨

在Elixir中，虽然我们有`String.length`函数来计算字符串长度，但是它并不是唯一的方法。我们也可以使用模式匹配来获取字符串的长度。

```Elixir
string = "Hello World"
length = String.length(string)

# 使用模式匹配获取长度
[_|rest] = string
IO.puts length
```

上面的代码将输出`11`，因为我们通过匹配`string`变量来获取了它的长度。当然，这种方法只适用于ASCII字符。

此外，我们也可以使用`Enum.count`函数来计算字符串的长度。这个函数接受一个函数作为参数，然后将字符串拆分成字符列表并对其进行计数。

```Elixir
string = "Hello World"
IO.puts Enum.count(string, fn _ -> true end)
```

这个例子将输出`11`，因为我们将每个字符都匹配为`true`，然后对整个字符列表进行计数。

# 参考链接

- [Elixir官方文档](https://hexdocs.pm/elixir/String.html)
- [Learn Elixir - Strings](https://elixir-lang.org/getting-started/string.html)
- [Elixir Tips](https://myelixirstatus.com/)