---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么和为什么？
打印调试输出是一种开发者跟踪和了解程序正在执行什么操作的方法。我们使用这种方法，以便在debug时更易于理解程序的运行状态和流程。

## 如何去做：
我们在代码中添加一些 print 语句，这些 print 语句提供关于正在发生的操作的即时反馈。看着下面的 Go 代码：

```Go
package main
import "fmt"

func main() {
    fmt.Println("Debug: 开始执行...")
    for i := 0; i<5; i++ {
        fmt.Println("Debug: 循环次数:", i)
    }
    fmt.Println("Debug: 执行完成.")
}
```

当你运行这段代码时，会打印出：

```
Debug: 开始执行...
Debug: 循环次数: 0
Debug: 循环次数: 1
Debug: 循环次数: 2
Debug: 循环次数: 3
Debug: 循环次数: 4
Debug: 执行完成.
```

## 深度剖析
**历史背景**：打印debug输出几乎和编程一样老，在早期没有像今天这样复杂的调试工具时，这是程序员唯一能够理解程序内部状况的方法。

**替代方案**：如今，我们有了更多的debug工具，例如内置debug工具，也可以添加专用的debug库，如 Delve，它们能以更有效的方式展示程序状态。

**实现细节**：`fmt.Println` 是 Go 中用于输出的一种方式，它可以被重定向到任何 `io.Writer`，包括文件、网络等。

## 另请参阅
- Delve：专用的 Go 调试库, [Delve GitHub](https://github.com/go-delve/delve)
- Go 官方「Effective Go」关于打印和日志的文档, [Effective Go Printing](https://golang.org/doc/effective_go#printing)