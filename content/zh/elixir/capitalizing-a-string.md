---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么及为什么？)
字符串首字母大写就是把开头的小写字母变成大写字母。程序员这样做通常是为了格式化文本数据，比如人名或标题。

## How to (如何操作):
```elixir
# Elixir 中的字符串首字母大写
string = "elixir programming language"
capitalized_string = String.capitalize(string)

IO.puts(capitalized_string) # 输出: "Elixir programming language"
```

```elixir
# 对列表中的所有字符串首字母大写
strings = ["elixir", "programming", "language"]
capitalized_strings = Enum.map(strings, &String.capitalize/1)

IO.inspect(capitalized_strings) # 输出: ["Elixir", "Programming", "Language"]
```

## Deep Dive (深入探讨):
Elixir 使用 UTF-8 编码，这意味着它天生支持多语言文本处理。首字母大写功能在很早前就存在于许多语言中，因为它用于人名、地名和文本开头，符合书写规范。

除了`String.capitalize/1`，还可以使用`String.upcase/1`来把整个字符串转成大写，或者`String.downcase/1`转成小写。

关于实现，首字母大写的功能会检测字符串的第一个字符，如果它是小写字母，则将其转为大写。Elixir处理字符串的方式允许快速且准确地操作文字，尤其是在涉及Unicode字符时。

## See Also (另见资源):
- Elixir官方文档的[String模块](https://hexdocs.pm/elixir/String.html)
- [Elixir School](https://elixirschool.com/en/) - 提供Elixir编程语言的教程
- [Programming Elixir](https://pragprog.com/book/elixir16/programming-elixir-1-6) - 一本关于Elixir编程的书，适合想深入了解语言的读者
