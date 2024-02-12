---
title:                "获取字符串的长度"
aliases:
- /zh/ruby/finding-the-length-of-a-string/
date:                  2024-01-20T17:48:07.807150-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在Ruby中找出字符串的长度就是确定它包含多少个字符。这对检查输入，截断文本或者在数据处理中保持一致性很有用。

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
