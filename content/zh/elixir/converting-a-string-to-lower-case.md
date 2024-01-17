---
title:                "把字符串转换为小写"
html_title:           "Elixir: 把字符串转换为小写"
simple_title:         "把字符串转换为小写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是转换字符串为小写，为什么程序员要这么做？
转换字符串为小写是指将字符串中的所有字母转换为小写形式。程序员经常这么做是因为在处理字符串时，有时候需要忽略大小写的差异，以使得比较和搜索更加容易和准确。

## 如何进行转换：
```Elixir
# 使用String.downcase函数将字符串转换为小写
String.downcase("HELLO WORLD") #=> "hello world"

# 不区分大小写的比较
"hello" == "Hello" #=> true
``` 

## 深入挖掘：
转换字符串为小写的操作在计算机编程中已经存在了很长一段时间。在早期的计算机系统中，由于存储和处理大写字母所需要的资源更多，所以小写字母更受欢迎。在今天的编程语言中，转换字符串为小写的操作也被广泛应用于处理用户输入和字符串比较的场景。除了使用String.downcase函数之外，也可以通过使用正则表达式或者编写自定义的实现来实现字符串转换为小写的功能。

## 延伸阅读：
- Elixir的String模块文档：https://hexdocs.pm/elixir/String.html
- 关于正则表达式的介绍：https://www.regular-expressions.info/