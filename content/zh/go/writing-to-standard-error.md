---
title:                "向标准错误写入"
html_title:           "Go: 向标准错误写入"
simple_title:         "向标准错误写入"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

什么和为什么？
写入标准错误是指将错误消息写入程序的标准错误输出流。程序员这样做的原因是为了能够通过读取此输出来识别和调试错误。

如何：
```Go
fmt.Fprintln(os.Stderr, "这是一个标准错误示例。")
```

输出：
```
这是一个标准错误示例。
```

深入探讨：
写入标准错误的概念最早出现在操作系统的早期版本中。如今，许多编程语言都具有写入标准错误的功能。除了使用fmt包中的Fprintln函数外，程序员还可以使用log包来记录错误消息。实际上，写入标准错误的方法取决于具体的编程语言和工具。

参考：
- [Go标准库文档](https://golang.org/pkg/)：可以查看fmt和log包的详细说明。
- [写入标准错误的历史背景](https://en.wikipedia.org/wiki/Standard_error)：可以了解更多关于标准错误的历史和用途。