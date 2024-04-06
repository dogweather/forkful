---
date: 2024-01-20 17:48:07.807150-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u957F\u5EA6\
  \u5B9A\u4E49\u4E86\u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7684\u6570\u91CF\u3002\u8BF4\
  \u5386\u53F2\uFF0CRuby\u7684`length`\u548C`size`\u65B9\u6CD5\u4ECE\u6700\u521D\u7248\
  \u672C\u5C31\u5B58\u5728\uFF0C\u5B83\u4EEC\u662F\u4E92\u6362\u7684\u3002\u66FF\u4EE3\
  \u65B9\u6848\u6709\u4E9B\uFF0C\u6BD4\u5982\u81EA\u5DF1\u904D\u5386\u5B57\u7B26\u4E32\
  \u8BA1\u7B97\u5B57\u7B26\uFF0C\u4F46\u8FD9\u6CA1\u5FC5\u8981\u4E14\u6548\u7387\u4F4E\
  \u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.637879-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u957F\u5EA6\u5B9A\u4E49\
  \u4E86\u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7684\u6570\u91CF\u3002\u8BF4\u5386\u53F2\
  \uFF0CRuby\u7684`length`\u548C`size`\u65B9\u6CD5\u4ECE\u6700\u521D\u7248\u672C\u5C31\
  \u5B58\u5728\uFF0C\u5B83\u4EEC\u662F\u4E92\u6362\u7684\u3002\u66FF\u4EE3\u65B9\u6848\
  \u6709\u4E9B\uFF0C\u6BD4\u5982\u81EA\u5DF1\u904D\u5386\u5B57\u7B26\u4E32\u8BA1\u7B97\
  \u5B57\u7B26\uFF0C\u4F46\u8FD9\u6CA1\u5FC5\u8981\u4E14\u6548\u7387\u4F4E."
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (如何操作：)
```Ruby
# 使用 String#length 方法
greeting = "你好, 世界!"
puts greeting.length
# 输出: 8

# 或使用 String#size 方法
puts greeting.size
# 输出: 8
```

## Deep Dive (深入探究)
字符串长度定义了字符串中字符的数量。说历史，Ruby的`length`和`size`方法从最初版本就存在，它们是互换的。替代方案有些，比如自己遍历字符串计算字符，但这没必要且效率低。

Ruby是用C语言写的，`length`方法实际上在字符串的内部数据结构中查找一个叫做`RSTRING_LEN`的字段来取得字符的数量。因为Ruby支持多语言字符，它计算的是字符的多字节表示，而不仅是字节。

## See Also (另见)
- [Ruby API Documentation for String](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby Style Guide](https://github.com/rubocop/ruby-style-guide)
