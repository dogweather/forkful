---
title:                "字符串插值"
aliases:
- /zh/ruby/interpolating-a-string.md
date:                  2024-01-20T17:51:27.903493-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要用？)
字符串插值是把变量或表达式嵌入到字符串中的方法。程序员这么做是为了动态构建字符串，便捷地创建具有不同数据的信息。

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
