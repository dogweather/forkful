---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
什么是正则表达式？简单来说就是搜索和替换文本的格式化工具。为什么程序员要用它？因为它强大、灵活，还能让代码更简洁。

## How to:
```Elixir
# 创建正则表达式
regex = ~r/hello/

# 匹配字符串
"hello world" =~ regex
# 输出: true

# 查找匹配
Regex.scan(regex, "hello world")
# 输出: [["hello"]]

# 替换文本
Regex.replace(regex, "hello world", "hi")
# 输出: "hi world"
```

## Deep Dive
正则表达式起源于20世纪50年代的神经生物学研究。现在，几乎所有编程语言都支持。Elixir中使用Regex模块进行操作，该模块底层基于Erlang的re模块。除了正则表达式，Elixir也可以使用模式匹配和String模块方法处理字符串，但正则表达式在处理复杂文本时更有优势。

## See Also
- [Elixir Regex 文档](https://hexdocs.pm/elixir/Regex.html)
- [在线正则表达式测试器](https://regex101.com/)
