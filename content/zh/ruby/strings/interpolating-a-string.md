---
date: 2024-01-20 17:51:27.903493-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u63D2\u503C\
  \u5728\u5386\u53F2\u4E0A\u4E00\u76F4\u662F\u4E00\u4E2A\u65B9\u4FBF\u4E14\u5F3A\u5927\
  \u7684\u5DE5\u5177\u3002Ruby\u81EA\u8BDE\u751F\u4EE5\u6765\u5C31\u652F\u6301\u8FD9\
  \u4E00\u529F\u80FD\u3002\u63D2\u503C\u53EA\u5728\u53CC\u5F15\u53F7\u5B57\u7B26\u4E32\
  \u4E2D\u6709\u6548\uFF1B\u5355\u5F15\u53F7\u5B57\u7B26\u4E32\u4E0D\u652F\u6301\u3002\
  \u5728#{...}\u5185\u90E8\uFF0C\u4F60\u53EF\u4EE5\u653E\u7F6E\u4EFB\u4F55Ruby\u4EE3\
  \u7801\uFF0C\u5305\u62EC\u53D8\u91CF\u3001\u7B97\u672F\u8868\u8FBE\u5F0F\u6216\u65B9\
  \u6CD5\u8C03\u7528\u3002 \u53E6\u4E00\u79CD\u65B9\u6CD5\u662F\u8FDE\u63A5\u5B57\u7B26\
  \u4E32\uFF0C\u6BD4\u5982 `\"Hello \" + name +\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.494292-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u63D2\u503C\u5728\u5386\
  \u53F2\u4E0A\u4E00\u76F4\u662F\u4E00\u4E2A\u65B9\u4FBF\u4E14\u5F3A\u5927\u7684\u5DE5\
  \u5177\u3002Ruby\u81EA\u8BDE\u751F\u4EE5\u6765\u5C31\u652F\u6301\u8FD9\u4E00\u529F\
  \u80FD\u3002\u63D2\u503C\u53EA\u5728\u53CC\u5F15\u53F7\u5B57\u7B26\u4E32\u4E2D\u6709\
  \u6548\uFF1B\u5355\u5F15\u53F7\u5B57\u7B26\u4E32\u4E0D\u652F\u6301\u3002\u5728#{...}\u5185\
  \u90E8\uFF0C\u4F60\u53EF\u4EE5\u653E\u7F6E\u4EFB\u4F55Ruby\u4EE3\u7801\uFF0C\u5305\
  \u62EC\u53D8\u91CF\u3001\u7B97\u672F\u8868\u8FBE\u5F0F\u6216\u65B9\u6CD5\u8C03\u7528\
  \u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## How to: (如何操作：)
```Ruby
# 字符串插值基础使用
name = "小明"
greeting = "你好, #{name}!"
puts greeting  # 输出: 你好, 小明!

# 使用表达式
hours_worked = 42
puts "这周工作了#{hours_worked}小时."  # 输出: 这周工作了42小时.

# 如果不是字符串而是数字的话
total = 23 * 8
puts "总数是#{total}"  # 输出: 总数是184
```

## Deep Dive (深入了解)
字符串插值在历史上一直是一个方便且强大的工具。Ruby自诞生以来就支持这一功能。插值只在双引号字符串中有效；单引号字符串不支持。在#{...}内部，你可以放置任何Ruby代码，包括变量、算术表达式或方法调用。
另一种方法是连接字符串，比如 `"Hello " + name + "!"`，这样也可以组成一个字符串，但不如插值直观或高效。
在Ruby内部，当解释器遇到一个带有插值的字符串时，它实际上是在每次遇到插值部分时按顺序执行这些代码片段，然后将其结果转换成字符串并连接起来。

## See Also (另请参阅)
- [Ruby 官方文档中的字符串插值](https://ruby-doc.org/core-3.1.0/doc/syntax/literals_rdoc.html#label-Strings)
- [Ruby风格指南中关于字符串插值的建议](https://rubystyle.guide/#string-interpolation)
