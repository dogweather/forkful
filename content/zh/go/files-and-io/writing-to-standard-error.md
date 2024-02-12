---
title:                "写入标准错误"
aliases:
- /zh/go/writing-to-standard-error/
date:                  2024-02-03T18:15:14.077210-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Go 中写入标准错误（stderr）涉及将错误消息或诊断信息导向不是用于主输出流的地方。程序员使用这种方式来将常规输出与错误信息分开，使得调试和日志解析更加直接。

## 如何做：

在 Go 中，`os` 包提供了代表标准错误文件的 `Stderr` 值。你可以将它与 `fmt.Fprint`、`fmt.Fprintf` 或 `fmt.Fprintln` 函数一起使用，以写入 stderr。这里有一个简单的示例：

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // 向 stderr 写入一个简单的字符串
    _, err := fmt.Fprintln(os.Stderr, "这是一个错误消息！")
    if err != nil {
        panic(err)
    }

    // 使用 Fprintf 进行格式化错误消息
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "进程带着 %d 个错误完成。\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

示例输出（到 stderr）：
```
这是一个错误消息！
进程带着 4 个错误完成。
```

记住，这些消息不会出现在常规输出（stdout）中，而是在错误流中，这在大多数操作系统中可以被单独重定向。

## 深入探讨

标准错误的概念深植于 Unix 哲学中，它清晰地区分了常规输出和错误消息，以便更高效地处理和管理数据。在 Go 中，通过 `os` 包拥抱这一传统，该包提供了直接访问 stdin、stdout 和 stderr 文件描述符的能力。

虽然直接写入到 `os.Stderr` 对许多应用程序来说是合适的，Go 还提供了更为复杂的日志记录包如 `log`，它提供了附加特性，例如时间戳和更灵活的输出配置（例如，写入到文件）。对于规模较大的应用程序或需要更全面的日志记录特性时，使用 `log` 包可能是一个更好的选择。值得注意的是，Go 对错误处理的方法，鼓励从函数返回错误，补充了将错误消息写入 stderr 的做法，允许进行更细粒度的错误管理和报告。

本质上，虽然在许多编程语言中写入 stderr 是一个基本任务，Go 的标准库和设计原则提供了处理错误输出的直接和高级路径，既符合更广泛的行业实践，同时也迎合了 Go 特有的设计理念。
