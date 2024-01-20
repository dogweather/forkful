---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 ＆ 为什么？
字符串插值是一种在字符串中插入变量或表达式的编程技术。程序员使用它来创建动态字符串，提高代码的可读性与效率。

## 如何使用：
我们来看一段 Elixir 中使用字符串插值的代码。

```elixir
name = "Jessica"
age = 24
IO.puts("Hello, my name is #{name} and I'm #{age} years old.")
```

运行以上代码，你会在输出中看到：

```
Hello, my name is Jessica and I'm 24 years old.
```

`#{}`中的代码是被计算和插入到字符串中的。

## 深入探讨
（1）历史背景：字符串插值几乎在所有现代编程语言中都存在，比如Ruby、Python和Javascript。而Elixir遵循此一通用实践。

（2）替代方案：如果不使用字符串插值，你也可以通过字符串连接来合并变量和字符串。但是这种方法代码看起来较为复杂和繁琐。

```elixir
IO.puts("Hello, my name is " <> name <> " and I'm " <> Integer.to_string(age) <> " years old.")
```

（3）实现详情：在Elixir中，字符串插值实际上由编译器在编译时展开为字符串连接。

## 更多参考
1. Elixir官方文档 [Strings in Elixir](https://elixir-lang.org/getting-started/sigils.html#strings)


3. [Elixir String Interpolation](https://hexdocs.pm/elixir/String.html#module-interpolation) 

希望这篇文章有助于你理解和应用Elixir中的字符串插值。