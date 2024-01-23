---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
输出错误信息到标准错误流(stderr)是一种打印日志、调试或报告错误的手段。程序员这么做是为了区分正常输出（stdout）和错误消息，方便调试和日志记录。

## How to: (如何操作)
在Ruby中，使用`$stderr.puts`或`STDERR.print`输出信息到标准错误。下面是例子：

```Ruby
# 使用puts方法输出到标准错误
$stderr.puts "发生错误！"

# 使用print方法输出到标准错误
STDERR.print "警告：操作无效！"
```

输出示例：

```
发生错误！
警告：操作无效！
```

## Deep Dive (深入探索)
标准错误（stderr）和标准输出（stdout）是UNIX和类UNIX系统的传统概念。它们将输出分为两个不同的渠道，使得用户能够独立地处理正常日志和错误日志。在Ruby中，你可以使用全局变量`$stderr`或常量`STDERR`写入错误信息。两者实质上是同一个对象的两个不同名称。除了直接写入stderr，Ruby还提供了`warn`方法作为向stderr输出警告信息的简便方式。

## See Also (另请参阅)
- [Ruby官方的IO文档](https://ruby-doc.org/core/IO.html)
- [关于标准流的讨论](https://en.wikipedia.org/wiki/Standard_streams)
- [`warn` 方法的使用](https://ruby-doc.org/core/Kernel.html#method-i-warn)
