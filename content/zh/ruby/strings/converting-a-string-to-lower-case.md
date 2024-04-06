---
date: 2024-01-20 17:39:06.180324-07:00
description: "How to: \u600E\u4E48\u505A \u8F6C\u6362\u5B57\u7B26\u4E32\u4E3A\u5C0F\
  \u5199\u5176\u5B9E\u5E76\u4E0D\u590D\u6742\u3002\u5728Ruby\u65E9\u671F\u7248\u672C\
  \uFF0C`.downcase` \u5C31\u5DF2\u7ECF\u88AB\u5F15\u5165\u3002\u8FD9\u4E2A\u65B9\u6CD5\
  \u662F\u7531 `String` \u7C7B\u63D0\u4F9B\u7684\uFF0C\u5B83\u80FD\u591F\u5904\u7406\
  \u591A\u79CD\u5B57\u7B26\u7F16\u7801\uFF0C\u5305\u62ECUTF-8\uFF0C\u8FD9\u5BF9\u4E8E\
  \u652F\u6301\u591A\u8BED\u8A00\u5E94\u7528\u662F\u91CD\u8981\u7684\u3002 \u53E6\u5916\
  \u7684\u65B9\u6CD5\uFF0C\u6BD4\u5982 `.downcase!`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.556461-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u8F6C\u6362\u5B57\u7B26\u4E32\u4E3A\u5C0F\u5199\u5176\
  \u5B9E\u5E76\u4E0D\u590D\u6742\u3002\u5728Ruby\u65E9\u671F\u7248\u672C\uFF0C`.downcase`\
  \ \u5C31\u5DF2\u7ECF\u88AB\u5F15\u5165\u3002\u8FD9\u4E2A\u65B9\u6CD5\u662F\u7531\
  \ `String` \u7C7B\u63D0\u4F9B\u7684\uFF0C\u5B83\u80FD\u591F\u5904\u7406\u591A\u79CD\
  \u5B57\u7B26\u7F16\u7801\uFF0C\u5305\u62ECUTF-8\uFF0C\u8FD9\u5BF9\u4E8E\u652F\u6301\
  \u591A\u8BED\u8A00\u5E94\u7528\u662F\u91CD\u8981\u7684\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to: 怎么做
```Ruby
# 将字符串转换为小写
example_str = "Hello World!"
lowercase_str = example_str.downcase

puts lowercase_str
# 输出: hello world!
```
简单的一行代码可以做到。

## Deep Dive 深入探讨
转换字符串为小写其实并不复杂。在Ruby早期版本，`.downcase` 就已经被引入。这个方法是由 `String` 类提供的，它能够处理多种字符编码，包括UTF-8，这对于支持多语言应用是重要的。

另外的方法，比如 `.downcase!` ，具有相同的功能但是会在原地修改字符串，如果字符串没变则返回nil。别忘记Ruby是一门灵活的语言，你还可以使用 `.swapcase` 来交换字符串中所有字母的大小写。

实际实现方面，`.downcase` 方法会考虑到语言特有的大小写转换规则。例如，在土耳其语中，大写的 'I' 转换成小写并不是 'i'，而是 'ı'。这意味着`.downcase`不仅仅是ASCII码的转换。在内部，Ruby代码会检查编码，并适当地调整字符，确保正确的转换。

## See Also 相关链接
- Ruby官方文档中关于String类的说明: [String Class - Ruby-Doc.org](https://ruby-doc.org/core-3.1.2/String.html)
