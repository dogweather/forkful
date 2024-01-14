---
title:    "Elixir: 字符串连接"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么
字符串连接在编程中是一个常见的操作，它可以将多个字符串组合成一个单一的字符串。在Elixir中，字符串连接是一个重要的概念，因为它可以帮助我们构建复杂的数据结构和输出结果。无论是用于简单的字符串拼接还是更复杂的数据处理，掌握字符串连接技巧都是很有用的。

## 如何操作

要进行字符串连接，我们需要使用Elixir内置的`<>`操作符。下面给出一个简单的例子：

```
Elixir
name = "张三"
greeting = "你好"
message = greeting <> name
```
输出结果为：
```
你好张三
```
这样我们就可以将多个字符串组合起来，形成一个新的字符串。同时，我们也可以在字符串连接中使用变量、函数以及表达式，可以大大提高我们的代码灵活性。比如：

```
Elixir
number1 = 10
number2 = 20
sum = "结果是：" <> Integer.toString(number1 + number2)
```

输出结果为：
```
结果是：30
```

## 深入探讨

在Elixir中，`<>`操作符其实是调用了`Kernel.<>/2`函数。该函数可以接受任意类型的参数，包括数字、布尔值、列表、集合和元组。当接受的参数为字符串时，它们会被组合成一个单一的字符串；当接受的参数为其他类型时，它们会被自动转换为字符串后再进行组合。这就使得字符串连接更加强大和灵活。

另外，Elixir还提供了`...`操作符来进行多个字符串的连接，它会自动在每个字符串之间添加空格。比如：

```
Elixir
message = "Hello" ... "world"
```

输出结果为：
```
Hello world
```

## 参考链接

- Elixir官方文档: [https://elixir-lang.org/getting-started/string-interpolation-and-concatenation.html](https://elixir-lang.org/getting-started/string-interpolation-and-concatenation.html)
- Elixir字符串操作文档: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- 《Elixir in Action》: [https://www.manning.com/books/elixir-in-action](https://www.manning.com/books/elixir-in-action)

## 参见

如果您对Elixir字符串连接感兴趣，可能也会喜欢以下主题：

- Elixir字符串格式化：[https://elixirschool.com/lessons/basics/string-interpolation/](https://elixirschool.com/lessons/basics/string-interpolation/)
- Elixir列表和集合操作：[https://elixirschool.com/lessons/basics/enum-map-reduce/](https://elixirschool.com/lessons/basics/enum-map-reduce/)
- Elixir函数操作符：[https://elixirschool.com/lessons/advanced/function-composition/](https://elixirschool.com/lessons/advanced/function-composition/)