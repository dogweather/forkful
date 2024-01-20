---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
字符串转换为小写在编程中常常被用到，目的主要是为了消除因大小写差异而引起的数据不一致问题。对于用户输入等情况，转换为统一的小写可以消除潜在的错误和混淆。

## 如何做:
在Elixir中，我们可以使用`String.downcase/1`函数将字符串转换为小写。下面是代码示例与输出结果：

```Elixir
IO.puts String.downcase("Hello World")
```
输出结果：
```
hello world
```
## 深度探索:
字符串转换为小写的操作在计算机程序设计的早期阶段就已存在。这主要是因为，计算机对于大小写敏感，而人类对于大小写的识别则相对容易混淆。除了`String.downcase/1`，Elixir还提供了其他一些处理字符串大小写的函数，如`String.upcase/1`（转换为大写），`String.capitalize/1`（首字符大写，其余小写）。

在Elixir的实现中，`String.downcase/1`不仅支持ASCII字符，还支持Unicode字符。这一特性使得Elixir在处理多语言字符串时有很好的兼容性。

## 另请参阅:
- Elixir官方文档的[String module section](https://hexdocs.pm/elixir/String.html)中有关于`String.downcase/1`函数以及其他字符串处理函数的详细说明。
- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)这本书中有关于Elixir字符串处理的深入讲解。