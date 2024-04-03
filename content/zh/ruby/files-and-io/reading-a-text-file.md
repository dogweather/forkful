---
date: 2024-01-20 17:55:04.913708-07:00
description: "\u5982\u4F55\u505A\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.392922-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## 如何做：
```Ruby
# 基本读取
File.open("example.txt", "r") do |file|
  puts file.read
end

# 按行读取
File.foreach("example.txt") { |line| puts line }

# 一次性读取全部内容
content = File.read("example.txt")
puts content
```

输出示例（以"example.txt"文件内容为“你好，世界！”为例）：
```
你好，世界！
你好，世界！
你好，世界！
```

## 深入探究
早期，数据存储和检索是编程的核心问题。Ruby的文件读取API遗传了Unix的思想，简洁高效。除了上文提到的方法，`IO.readlines`和`File.readlines`可以直接返回文件的所有行。这些方法都涉及IO（输入/输出）对象。在实现上，Ruby在C语言的标准库基础上，提供了易于使用的面向对象封装。

## 参考链接
- “Learn Ruby the Hard Way” 关于文件操作的章节: [Exercise 15: Reading Files](https://learnrubythehardway.org/book/ex15.html)
