---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
正则表达式是一种文字模式匹配的工具。程序员用它来搜索、编辑或处理复杂的文本数据。

## How to (如何操作)
```Ruby
# 查找第一个匹配
puts "hello world".match(/o/)

# 全局匹配
puts "hello world".scan(/l/)

# 替换文本
puts "hello world".sub(/o/, "0")

# 替换所有匹配的文本
puts "hello world".gsub(/l/, "1")
```

输出:
```
o
["l", "l", "l"]
hell0 world
he11o wor1d
```

## Deep Dive (深入探索)
正则表达式的历史可以追溯到20世纪50年代。替代品包括纯字符串查找和处理函数，但它们不如正则表达式强大。在Ruby中，通过`Regexp`类实现正则表达式，它提供了灵活和强大的模式匹配功能。

## See Also (另请参阅)
- Ruby官方文档中关于Regexp的介绍：[Ruby Regexp](https://ruby-doc.org/core-3.1.2/Regexp.html)
- 正则表达式的互动教程：[Rubular](http://rubular.com/)
- 正则表达式学习指南：[Regular-Expressions.info](https://www.regular-expressions.info/)
