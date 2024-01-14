---
title:                "Elixir: 使用正则表达式"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么使用正则表达式？

正则表达式是一种强大的工具，它可以在字符串中搜索和匹配特定的模式。它们在编程中经常被用于验证输入，过滤数据，和搜索特定的文本。使用正则表达式可以节省时间和提高代码的可读性，因此成为潜在的编程利器。

## 如何使用 Elixir 编写正则表达式？

```Elixir
# 使用 =~ 运算符来匹配字符串和模式
"hello" =~ ~r/hello/

# 也可以使用 Regex 模块来创建正则表达式
regex = Regex.compile(~r/hello/)
"hello" =~ regex

# 使用 |> 和 match? 函数来在管道中匹配字符串
"hello elixir" |> match?(~r/elixir/)

# 使用正则表达式替换字符串中的文本
"hello" |> Regex.replace(~r/hello/, "ola")

# 可以使用 =~ 运算符的 ! 版本来抛出错误，便于处理错误情况
input = "12345"
~r/\d{5}/ =~ input || raise("Invalid input format") 
```

## 深入了解正则表达式

正则表达式提供了许多特殊的字符和表达式来方便匹配和搜索字符串。例如，可以使用 "[]" 来匹配一组特定的字符，使用 "*" 表示匹配前一个字符零次或多次，使用 "+" 表示匹配前一个字符一次或多次。regex 模块还提供功能强大的选项如全局匹配和忽略大小写。

## 参考链接

[正则表达式教程（Elixir）](https://elixir-lang.org/getting-started/advanced/string-patterns.html)

[正则表达式无痛入门指南（Mandarin）](https://qnimate.com/an-introduction-to-regular-expressions-regex-in-elixir/)

[官方 Regex 模块文档](https://hexdocs.pm/elixir/Regex.html)

## 参见