---
date: 2024-01-20 17:56:38.557198-07:00
description: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\
  \u6839\u636E\u7528\u6237\u8F93\u5165\u505A\u51FA\u53CD\u5E94\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u7A0B\u5E8F\u66F4\u7075\u6D3B\uFF0C\u53EF\
  \u4EE5\u5904\u7406\u4E0D\u540C\u7684\u60C5\u51B5\u548C\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.390727-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\
  \u6839\u636E\u7528\u6237\u8F93\u5165\u505A\u51FA\u53CD\u5E94\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u7A0B\u5E8F\u66F4\u7075\u6D3B\uFF0C\u53EF\
  \u4EE5\u5904\u7406\u4E0D\u540C\u7684\u60C5\u51B5\u548C\u4EFB\u52A1\u3002."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## What & Why? (是什么？为什么？)
读取命令行参数可以让程序根据用户输入做出反应。程序员这么做是为了让程序更灵活，可以处理不同的情况和任务。

## How to: (怎么做：)
在Ruby中，我们利用`ARGV`数组来接收命令行参数。这是个简单的范例：

```Ruby
# hello.rb
name = ARGV.first || "世界"
puts "你好，#{name}!"
```

运行它：

```
$ ruby hello.rb
你好，世界!

$ ruby hello.rb 格雷
你好，格雷!
```

## Deep Dive (深入探索)
命令行参数是一种老派但强大的技术，它从早期的Unix时代就开始用了。它让脚本和程序在执行时能够接收参数，没有图形界面时，这是必不可少的交互方式。在Ruby中，除了`ARGV`，还有其他解析命令行参数的库，比如`OptionParser`和`Thor`，它们可以管理更复杂的命令行选项。实际上，`ARGV`是一个全局数组，Ruby解释器在开始前就已经填充了它。

## See Also (另请参阅)
- Ruby官方文档关于`ARGV`: [https://ruby-doc.org/core-2.5.1/ARGF.html#method-c-argv](https://ruby-doc.org/core-2.5.1/ARGF.html#method-c-argv)
- `OptionParser`官方文档: [https://ruby-doc.org/stdlib-2.5.1/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.5.1/libdoc/optparse/rdoc/OptionParser.html)
- 关于`Thor`的介绍和教程: [https://github.com/rails/thor](https://github.com/rails/thor)
