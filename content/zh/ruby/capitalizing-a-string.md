---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串大写化，就是将字符串开头的字符变成大写字母。程序员这样做通常是为了格式化文本，以符合标题、姓名或其他需要特定格式的场合。

## How to: (如何操作：)
```Ruby
# 使用 capitalize 方法
str = "hello world"
puts str.capitalize
# 输出：Hello world

# 使用 capitalize! 方法直接修改原字符串
str = "hello world"
str.capitalize!
puts str
# 输出：Hello world
```

## Deep Dive (深入探讨)
在 Ruby 中，字符串大写化很简单，使用 `.capitalize` 或 `.capitalize!` 即可。`.capitalize` 会创建一个新的字符串，而 `.capitalize!` 会修改原字符串。要注意的是，只有首个字母会变大写，其余部分会变成小写。

历史上，字符串处理一直是编程语言的基础功能。在早期的编程语言中，如 COBOL 或 FORTRAN，字符串大写化需要手动操作每个字符。Ruby 的 `.capitalize` 让这个过程便捷多了。

替代方法？当然。你可以使用 `.sub` 方法配合正则表达式，或者手动修改每个字符。不过，为什么要走弯路呢？

实现细节方面，`.capitalize` 方法检查字符串的第一个字符，如果它是小写字母，就将其转换成大写。剩余字符则无条件转换为小写。

## See Also (参考链接)
- Ruby 文档中的 String 类： [Ruby-Doc String](https://ruby-doc.org/core/String.html)
- 关于 Ruby 字符串处理的教程：[Ruby Learning](http://rubylearning.com/satishtalim/string_operations_in_ruby.html)

记住：每个工具重在合适。学会何时何地使用 `.capitalize` 是成为高效 Ruby 程序员的一部分。
