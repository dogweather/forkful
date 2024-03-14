---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:28.020219-07:00
description: "\u5728 Go \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u6D89\u53CA\
  \u5230\u4ECE\u7EC8\u7AEF\u6216\u547D\u4EE4\u63D0\u793A\u7B26\u8C03\u7528\u7A0B\u5E8F\
  \u65F6\u63D0\u53D6\u63D0\u4F9B\u7ED9\u7A0B\u5E8F\u7684\u53C2\u6570\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u7684\
  \u60C5\u51B5\u4E0B\u81EA\u5B9A\u4E49\u7A0B\u5E8F\u6267\u884C\uFF0C\u4F7F\u5E94\u7528\
  \u7A0B\u5E8F\u66F4\u52A0\u7075\u6D3B\u548C\u7531\u7528\u6237\u9A71\u52A8\u3002"
lastmod: '2024-03-13T22:44:47.162945-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Go \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u6D89\u53CA\u5230\
  \u4ECE\u7EC8\u7AEF\u6216\u547D\u4EE4\u63D0\u793A\u7B26\u8C03\u7528\u7A0B\u5E8F\u65F6\
  \u63D0\u53D6\u63D0\u4F9B\u7ED9\u7A0B\u5E8F\u7684\u53C2\u6570\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u7684\u60C5\
  \u51B5\u4E0B\u81EA\u5B9A\u4E49\u7A0B\u5E8F\u6267\u884C\uFF0C\u4F7F\u5E94\u7528\u7A0B\
  \u5E8F\u66F4\u52A0\u7075\u6D3B\u548C\u7531\u7528\u6237\u9A71\u52A8\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## 什么与为什么？

在 Go 中读取命令行参数涉及到从终端或命令提示符调用程序时提取提供给程序的参数。程序员这样做是为了在不改变代码的情况下自定义程序执行，使应用程序更加灵活和由用户驱动。

## 如何做：

Go 通过 `os` 包直接访问命令行参数，特别是使用 `os.Args`，一个字符串数组。这里有一个简单的例子来帮我们开始：

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args 提供对原始命令行参数的访问
    fmt.Println("命令行参数:", os.Args)

    if len(os.Args) > 1 {
        // 循环遍历参数，跳过第一个（程序名）
        for i, arg := range os.Args[1:] {
            fmt.Printf("参数 %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("未提供命令行参数。")
    }
}
```

当运行 `go run yourprogram.go arg1 arg2` 时的示例输出可能看起来像：

```
命令行参数: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
参数 1: arg1
参数 2: arg2
```

这会打印出所有参数，包括程序名（通常在索引 0 处），然后遍历每个提供的参数，将它们打印出来。对于更受控的参数解析，你可能会考虑使用 `flag` 包来解析命令行选项。

## 深入探索

从历史上看，访问命令行参数是一种和 C 编程一样古老的实践，在那里 `argc` 和 `argv[]` 有类似的作用。在 Go 中，`os.Args` 使之简单直接，但有意地保持基础。对于更复杂的场景，例如处理标志或选项，Go 提供了 `flag` 包，它提供了强大的解析能力。当你的应用程序需要不仅仅是位置参数时，这可以被看作是一个“更好”的选择。

与一些脚本语言不同，这些语言提供了将命令行参数解析为关联数组或对象的内置功能，Go 的方法要求程序员使用 `os.Args` 手动处理基本需求的解析，或者为了更高级的场景利用 `flag` 包。这种设计反映了 Go 的哲学——保持核心语言的简单，同时为常见任务提供强大的标准库。虽然这可能为习惯了内置解析的人带来一点学习曲线，但它提供了更大的灵活性，并鼓励对命令行参数处理有更深入的理解。
