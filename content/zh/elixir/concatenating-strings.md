---
title:                "字符串拼接"
date:                  2024-01-20T17:34:22.093752-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串拼接就是把两个或多个字符串合并成一个。程序员拼接字符串是为了创建新的文本信息或动态生成代码。

## How to (操作方法)
在Elixir里，你可以用`<>/2`运算符来拼接字符串。看下面的例子：

```elixir
str1 = "您好，"
str2 = "世界！"
result = str1 <> str2
IO.puts result
```

输出将会是：

```
您好，世界！
```

## Deep Dive (深入了解)
字符串在Elixir中是UTF-8编码的二进制。历史上，不同的语言采用了不同的字符串拼接机制。比如，C语言使用了字符串函数，Java则使用了StringBuilder类。

在Elixir中，`<>/2`运算符在底层是通过Erlang虚拟机的优化来实现的。避免无谓的字符串拷贝，保证操作的效率。当然，还有其他方法可以拼接，比如使用字符串插值：

```elixir
name = "世界"
IO.puts "您好，#{name}！"
```

但是对于大量的字符串拼接操作，建议使用`<>/2`，因为它更加高效。

## See Also (另请参阅)
- Elixir官方文档上的[String模块](https://hexdocs.pm/elixir/String.html)
- [Erlang的Efficiency Guide关于二进制的章节](http://erlang.org/doc/efficiency_guide/binaryhandling.html)
- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) 这本书提供了更深入的Elixir编程知识。
