---
title:                "Elixir: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取子串是一项非常有用的技能，可以帮助我们在字符串中找出特定的信息。无论是处理字符串的输入，还是从已有的字符串中提取变量，提取子串都能够让我们的程序变得更加灵活和高效。在接下来的部分，我们将展示如何在Elixir中提取子串，以及一些有用的技巧。

## 如何操作

提取子串的方法有很多种，这取决于我们想要提取的内容。在Elixir中，我们可以使用 `String.slice` 函数来提取一个字符串的一部分。下面是一个简单的例子：

```Elixir
str = "Hello World"
String.slice(str, 0, 5)
```

这个例子中，我们从 "Hello World" 这个字符串中提取了前五个字符，通过指定起始位置和截取长度来实现。输出结果为 "Hello"。我们还可以通过正则表达式来提取更复杂的子串，比如匹配邮箱地址中的用户名：

```Elixir
str = "john@example.com"
Regex.run(~r/([a-z0-9]+)@/, str) |> List.first |> hd
```

这个例子中，我们首先使用正则表达式来匹配 "john@example.com" 中的用户名部分，然后通过 `List.first` 和 `hd` 函数来提取出这个用户名。输出结果为 "john"。

## 深入探究

除了提取字符串的一部分，我们还可以使用 Elixir 中的模式匹配来提取子串。模式匹配可以让我们更灵活地提取出所需的信息，比如从一个包含多个字段的字符串中提取出特定的字段。

下面是一个例子：

```Elixir
str = "Name: John, Age: 25, Email: john@example.com"
[_, name, _, _, age, _, _, email] = ~r/Name: (.*), Age: (\d+), Email: (.*)/.match?(str)

IO.puts "Name: #{name} \nAge: #{age} \nEmail: #{email}"
```

通过使用模式匹配，我们可以提取出字符串中的姓名、年龄和邮箱地址，然后通过 `IO.puts` 函数打印出来。输出结果为：

```
Name: John 
Age: 25 
Email: john@example.com
```

## 参考资料

- [Elixir官方文档: String.slice](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir官方文档: Regex](https://hexdocs.pm/elixir/Regex.html)
- [Elixir字符串处理指南](https://elixir-lang.org/getting-started/string.html)
- [Elixir字符串与模式匹配](https://elixirschool.com/lessons/basics/string-pattern-matching/)

### 参见

- [Elixir官方文档](https://elixir-lang.org/docs.html)
- [Elixir中国社区](https://elixir-cn.com/)
- [Elixir语言学习指南](https://www.douban.com/note/744567983/)