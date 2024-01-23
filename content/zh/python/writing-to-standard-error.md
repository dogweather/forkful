---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
在Python中，往标准错误（stderr）写数据是把程序中的错误信息输出到一个特定的通道，这样用户就能分清正常消息和错误消息。程序员这么做是为了调试程序更方便，同时让正常输出和错误输出分开，便于重定向和日志记录。

## How to: (如何做：)
使用Python写入标准错误很简单。看例子：

```Python
import sys

# 正常输出
print("这是正常输出的信息。")

# 错误输出
print("这是错误输出的信息。", file=sys.stderr)

# 例子输出：（注意，实际上错误信息可能会先显示，这取决于缓冲区的处理方式）
# 这是正常输出的信息。
# 这是错误输出的信息。
```

## Deep Dive (深入了解)
- 历史背景：UNIX哲学中强调了工具链和组合的概念，其中‘stderr’是一个重要部分，用于分离普通输出和错误。
- 替代方案：有时候，你可能会用日志库（比如`logging`），或是直接打开一个文件写入错误信息，但`sys.stderr`是最直接的标准错误输出方式。
- 实现细节：在底层，标准错误是一个文件描述符，它默认指向控制台，但它可以被重定向到文件或其他设备。

## See Also (另请参阅)
- Python官方文档：[sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- Python官方文档：[logging](https://docs.python.org/3/library/logging.html)
- Wikipedia上有关UNIX哲学的文章：[Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)
