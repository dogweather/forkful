---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么? (What & Why?)

提取子字符串是获取字符串部分的过程。程序员这样做的原因是需要对特定部分的字符串进行操作或分析。

## 如何操作: (How to:)

在Ruby中，你可以使用索引或“slice”方法提取子字符串。以下是一些例子：


```Ruby
str = "Hello, World!"
  
# 使用索引
puts str[0,5]  # 输出: "Hello"

# 使用slice方法
puts str.slice(7,5)  # 输出: "World"
```

要提取字符串的一部分，只需指定开始位置和长度即可。

## 深入剖析 (Deep Dive):

提取子字符串的概念已经存在了很长时间，早在20世纪60年代初的早期编程语言如 COBOL 中就已存在。后来的编程语言，例如 Python 还提供了更多的方法，如使用正则表达式。

此外，Ruby内部存储字符串为字节序列。当提取子字符串时，Ruby根据给定的开始位置和长度，获取相应的字节序列，并创建一个新的字符串对象。

## 参阅 (See Also):

1. Ruby 文档: [String - Ruby 2.7.0 原文档](https://docs.ruby-lang.org/en/2.7.0/String.html)
2. Ruby教程: [Ruby 字符串切片](https://www.runoob.com/ruby/ruby-string.html)