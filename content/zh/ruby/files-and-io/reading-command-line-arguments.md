---
date: 2024-01-20 17:56:38.557198-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u5728Ruby\u4E2D\uFF0C\u6211\u4EEC\
  \u5229\u7528`ARGV`\u6570\u7EC4\u6765\u63A5\u6536\u547D\u4EE4\u884C\u53C2\u6570\u3002\
  \u8FD9\u662F\u4E2A\u7B80\u5355\u7684\u8303\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.663993-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u5728Ruby\u4E2D\uFF0C\u6211\u4EEC\u5229\u7528\
  `ARGV`\u6570\u7EC4\u6765\u63A5\u6536\u547D\u4EE4\u884C\u53C2\u6570\u3002\u8FD9\u662F\
  \u4E2A\u7B80\u5355\u7684\u8303\u4F8B\uFF1A."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
