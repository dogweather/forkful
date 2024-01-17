---
title:                "把字符串转换为小写"
html_title:           "Ruby: 把字符串转换为小写"
simple_title:         "把字符串转换为小写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是字符串小写转换？为什么程序员会这么做？
字符串小写转换是一种将所有字母都转换为小写字母的操作，通常用于处理用户输入或数据清洗。程序员这么做的原因是为了匹配和统一数据，以及避免大小写敏感的问题。

## 如何进行字符串小写转换？
```Ruby
string = "Hello World"
puts string.downcase
```
这段代码将输出"hellow world"，字符串中所有的字母都被转换为小写字母。另外，还可以使用.string.downcase!来直接修改原字符串。

## 深入探讨
字符串小写转换的历史可以追溯到早期的编程语言，如C语言。此外，除了使用Ruby的内置方法进行转换外，还可以使用正则表达式或自定义方法来实现相同的功能。

## 参考资料
- Ruby文档：[Strings Downcase](https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase)
- 类似文章：[String Case Conversions in Ruby](https://www.geeksforgeeks.org/string-case-conversions-in-ruby/)