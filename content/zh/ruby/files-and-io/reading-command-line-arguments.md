---
title:                "读取命令行参数"
aliases:
- zh/ruby/reading-command-line-arguments.md
date:                  2024-01-20T17:56:38.557198-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
