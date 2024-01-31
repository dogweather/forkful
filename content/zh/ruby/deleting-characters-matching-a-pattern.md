---
title:                "匹配模式删除字符"
date:                  2024-01-20T17:42:54.009976-07:00
model:                 gpt-4-1106-preview
simple_title:         "匹配模式删除字符"

category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

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
