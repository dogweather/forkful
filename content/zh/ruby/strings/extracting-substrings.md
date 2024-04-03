---
date: 2024-01-20 17:46:23.236641-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u610F\u5473\u7740\u4ECE\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u4E2D\u53D6\u51FA\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5206\u6790\u3001\u641C\u7D22\u6216\u8F6C\
  \u6362\u7279\u5B9A\u7684\u6587\u672C\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.358320-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u610F\u5473\u7740\u4ECE\u4E00\u4E2A\
  \u5B57\u7B26\u4E32\u4E2D\u53D6\u51FA\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5206\u6790\u3001\u641C\u7D22\u6216\u8F6C\
  \u6362\u7279\u5B9A\u7684\u6587\u672C\u6570\u636E\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to: (如何执行)
Ruby提供了几种提取子字符串的方法。下面看几个例子：

```Ruby
# 使用 [] 和 slice 方法
str = "Hello, Ruby!"

substring1 = str[7, 4]      # 从位置7开始取4个字符
puts substring1              # => "Ruby"

substring2 = str.slice(0..4) # 使用范围对象获取子字符串
puts substring2              # => "Hello"

# 使用正则表达式和 match 方法
match_data = /R(.*)/.match(str)
puts match_data[1]           # => "uby!"
```

输出：
```
Ruby
Hello
uby!
```

## Deep Dive (深入探究)
提取子字符串在编程的世界里是基本功。在Ruby早期版本中，`[]` 和 `slice` 方法已被引入。两者可以互换，但 `[]` 方法使用更为频繁，因为它更简短更直观。

其他语言有类似Ruby中的 `[]` 方法。Python有切片(slice)，Java有 `substring()` 方法。它们的概念相同，只是实现方式有所不同。Ruby的 `slice` 方法可以说是受到Python切片的启发。

在Ruby的实现中，当用 `[]` 方法提取子字符串时，负数索引代表从字符串末尾开始计数。如果索引超出字符串范围，则返回 `nil`。

还可以使用正则表达式匹配模式提取子字符串。用 `match` 方法时，会返回一个MatchData对象，从中可以通过位置索引访问匹配的子字符串。

## See Also (另请参阅)
- Ruby官方文档关于字符串的章节: [String](https://ruby-doc.org/core-3.1.0/String.html)
- Ruby正则表达式: [Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
