---
title:                "Elixir: 字符串连接"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么: 为什么要把字符串连接起来？

## 如何: 使用Elixir连接字符串的例子和样例输出，显示在"```Elixir ... ```"的代码块中。

连接字符串是一个常见的编程需求，特别是在处理用户输入或从数据库中检索数据时。Elixir提供了简单且有效的方法来连接字符串，使您的代码更具表现力和可读性。下面是一个基本的例子：

```Elixir
name = "John"
greeting = "Hello"
full_greeting = greeting <> " " <> name
IO.puts(full_greeting)

# 输出：Hello John
```

在这个例子中，我们使用`<>`运算符来连接两个字符串。您还可以使用`++`运算符来连接字符串列表，例如：

```Elixir
phrases = ["Hello", "你好", "Bonjour"]
full_phrase = phrases ++ ["John"]
IO.puts(full_phrase)

# 输出：[Hello, 你好, Bonjour, John]
```

## 深入了解

连接字符串的`<>`运算符实际上是调用Elixir的`String.concat/2`函数。这个函数接受一个参数列表，并将它们连接成一个字符串。因此，您也可以这样做：

```Elixir
String.concat(["Hello", "John"])

# 输出：Hello John
```

除了`<>`和`++`运算符外，您还可以使用Elixir的`Enum.reduce/3`函数来连接字符串。这个函数接受一个可枚举的集合和一个可选的初始值，并按照指定的功能来减少集合中的元素。以下是使用`Enum.reduce/3`来连接字符串的例子：

```Elixir
phrases = ["Hello", "你好", "Bonjour"]
full_phrase = Enum.reduce(phrases, fn phrase, acc -> acc <> " " <> phrase end)
IO.puts(full_phrase)

# 输出：Hello 你好 Bonjour
```

在这个例子中，我们将每个短语添加到初始字符串的末尾，最终得到一个完整的字符串。

## 参考链接

- [Elixir Language Official Website](https://elixir-lang.org/)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/basics/#strings)
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)