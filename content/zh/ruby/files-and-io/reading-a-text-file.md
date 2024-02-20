---
date: 2024-01-20 17:55:04.913708-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u5C06\u6587\u4EF6\u5185\
  \u5BB9\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u4E3B\u8981\u662F\u4E3A\u4E86\u5904\u7406\u6570\u636E\uFF0C\u6BD4\u5982\u5206\u6790\
  \u3001\u8F6C\u6362\u6216\u8005\u663E\u793A\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.455954
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u5C06\u6587\u4EF6\u5185\
  \u5BB9\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u4E3B\u8981\u662F\u4E3A\u4E86\u5904\u7406\u6570\u636E\uFF0C\u6BD4\u5982\u5206\u6790\
  \u3001\u8F6C\u6362\u6216\u8005\u663E\u793A\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
读取文本文件就是将文件内容加载到程序中。程序员这样做主要是为了处理数据，比如分析、转换或者显示。

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
