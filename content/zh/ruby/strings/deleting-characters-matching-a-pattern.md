---
date: 2024-01-20 17:42:54.009976-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) \u5220\u9664\u5339\u914D\u6A21\u5F0F\
  \u7684\u5B57\u7B26\u8FD9\u4E2A\u529F\u80FD\u5728Ruby\u65E9\u671F\u7248\u672C\u5C31\
  \u5B58\u5728\u4E86\u3002`gsub` \u65B9\u6CD5\u7528\u4E8E\u5168\u5C40\u66FF\u6362\u5339\
  \u914D\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\u5B57\u7B26\uFF0C\u800C`strip`\u3001\
  `lstrip`\u548C`rstrip`\u65B9\u6CD5\u662F\u540E\u6765\u6DFB\u52A0\u7684\uFF0C\u7528\
  \u4E8E\u6D88\u9664\u5B57\u7B26\u4E32\u4E24\u7AEF\u6216\u4E00\u7AEF\u7684\u7A7A\u683C\
  \u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.491803-06:00'
model: gpt-4-1106-preview
summary: "\u4E00\u4E2A\u66FF\u4EE3\u65B9\u6848\u662F\u624B\u52A8\u5FAA\u73AF\u5B57\
  \u7B26\u4E32\u7684\u5B57\u7B26\u5E76\u6784\u5EFA\u4E00\u4E2A\u65B0\u7684\u4E0D\u5305\
  \u542B\u7279\u5B9A\u6A21\u5F0F\u7684\u5B57\u7B26\u4E32\uFF0C\u4F46\u8FD9\u79CD\u65B9\
  \u5F0F\u65E2\u8D39\u65F6\u53C8\u5BB9\u6613\u51FA\u9519\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

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
