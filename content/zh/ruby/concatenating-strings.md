---
title:                "连接字符串"
html_title:           "Ruby: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# #什么是字符串拼接？为什么程序员要这样做？

字符串拼接指的是将多个字符串连接在一起，形成一个新的更长的字符串。程序员经常这样做是因为在处理文本和数据的时候，经常需要将多个文本片段合并成一条完整的信息。

## #如何操作：

`` `Ruby
puts "Hello" + " " + "World"
`` `

输出： "Hello World"

`` `Ruby
greeting = "Hello"
name = "Ruby"
puts greeting + " " + name
`` `

输出： "Hello Ruby"

## #深入探讨：

1. 历史背景：字符串拼接是编程世界中常见的操作，早期的编程语言如C和Java都有相应的拼接函数。Ruby中使用+操作符来实现字符串拼接，使得代码更加简洁易读。
2. 其他方法：除了+操作符，Ruby还提供了几种其他方法来实现字符串拼接，如<<操作符和concat()函数。
3. 实现细节：在Ruby中，字符串是不可变的，意味着一旦被创建，就无法被修改。因此，当我们对字符串进行拼接操作时，实际上是创建了一个新的字符串，而原来的字符串仍然存在于内存中。

## #相关链接：

- Ruby官方文档：https://www.ruby-lang.org/zh_cn/documentation/
- 关于字符串拼接的更多信息：https://zh.wikipedia.org/wiki/%E5%AD%97%E7%AC%A6%E4%B8%B2%E6%8E%A5%E8%BD%AC