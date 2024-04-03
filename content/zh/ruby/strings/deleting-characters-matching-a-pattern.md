---
date: 2024-01-20 17:42:54.009976-07:00
description: "\u5728Ruby\u4E2D\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\
  \u80FD\u5E2E\u6211\u4EEC\u6E05\u6D17\u6570\u636E\uFF0C\u6BD4\u5982\u53BB\u9664\u65E0\
  \u7528\u7684\u7A7A\u683C\u6216\u7279\u6B8A\u7B26\u53F7\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u4E00\u81F4\u6027\uFF0C\u6216\
  \u8005\u4E3A\u4E86\u6EE1\u8DB3\u6570\u636E\u5B58\u50A8\u548C\u5904\u7406\u7684\u8981\
  \u6C42\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.352953-06:00'
model: gpt-4-1106-preview
summary: "\u5728Ruby\u4E2D\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\u80FD\
  \u5E2E\u6211\u4EEC\u6E05\u6D17\u6570\u636E\uFF0C\u6BD4\u5982\u53BB\u9664\u65E0\u7528\
  \u7684\u7A7A\u683C\u6216\u7279\u6B8A\u7B26\u53F7\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u4E00\u81F4\u6027\uFF0C\u6216\u8005\
  \u4E3A\u4E86\u6EE1\u8DB3\u6570\u636E\u5B58\u50A8\u548C\u5904\u7406\u7684\u8981\u6C42\
  \u3002."
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

## What & Why? (是什么？为什么？)
在Ruby中删除匹配模式的字符能帮我们清洗数据，比如去除无用的空格或特殊符号。程序员这么做通常是为了数据一致性，或者为了满足数据存储和处理的要求。

## How to (如何操作)
```Ruby
# 删除字符串中所有的数字
str = "My phone number is 12345."
clean_str = str.gsub(/\d/, '')
puts clean_str
# => My phone number is .

# 删除字符串开头和结尾的空格
trimmed_str = "   Hello, World!   ".strip
puts trimmed_str
# => Hello, World!

# 只删除字符串开头的空格
left_trimmed_str = "   Hello, World!   ".lstrip
puts left_trimmed_str
# => Hello, World!   

# 只删除字符串末尾的空格
right_trimmed_str = "   Hello, World!   ".rstrip
puts right_trimmed_str
# =>    Hello, World!
```

## Deep Dive (深入探索)
删除匹配模式的字符这个功能在Ruby早期版本就存在了。`gsub` 方法用于全局替换匹配正则表达式的字符，而`strip`、`lstrip`和`rstrip`方法是后来添加的，用于消除字符串两端或一端的空格。

一个替代方案是手动循环字符串的字符并构建一个新的不包含特定模式的字符串，但这种方式既费时又容易出错。

Ruby内部实现这些方法时进行了优化，特别是在处理Unicode字符时。使用内建的字符串处理方法，比如`gsub`和`strip`系列方法，比自己尝试操作字符串要快得多，也更可靠。

## See Also (另请参阅)
- Ruby官方文档[String#gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- Ruby官方文档[String#strip](https://ruby-doc.org/core-2.7.0/String.html#method-i-strip)
- [正则表达式Ruby教程](https://www.rubyguides.com/2015/06/ruby-regex/)
