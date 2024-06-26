---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:15.706213-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Go \u63D0\u4F9B\u4E86\u4E00\u4E2A\u5185\
  \u7F6E\u7684\u8C03\u8BD5\u5DE5\u5177\u53EB\u4F5C `delve`\u3002\u8FD9\u662F\u4E00\
  \u4E2A\u529F\u80FD\u9F50\u5168\u7684\u8C03\u8BD5\u5DE5\u5177\uFF0C\u5141\u8BB8\u60A8\
  \u9010\u6B65\u6267\u884C Go \u7A0B\u5E8F\uFF0C\u68C0\u67E5\u7A0B\u5E8F\u53D8\u91CF\
  \u548C\u8BC4\u4F30\u8868\u8FBE\u5F0F\u3002 \u9996\u5148\uFF0C\u60A8\u5FC5\u987B\u5B89\
  \u88C5 `delve`\u3002\u60A8\u53EF\u4EE5\u901A\u8FC7\u8FD0\u884C\u4EE5\u4E0B\u547D\
  \u4EE4\u6765\u5B8C\u6210\u5B89\u88C5\uFF1A."
lastmod: '2024-04-05T22:38:46.336175-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Go \u63D0\u4F9B\u4E86\u4E00\u4E2A\u5185\u7F6E\
  \u7684\u8C03\u8BD5\u5DE5\u5177\u53EB\u4F5C `delve`\u3002\u8FD9\u662F\u4E00\u4E2A\
  \u529F\u80FD\u9F50\u5168\u7684\u8C03\u8BD5\u5DE5\u5177\uFF0C\u5141\u8BB8\u60A8\u9010\
  \u6B65\u6267\u884C Go \u7A0B\u5E8F\uFF0C\u68C0\u67E5\u7A0B\u5E8F\u53D8\u91CF\u548C\
  \u8BC4\u4F30\u8868\u8FBE\u5F0F\u3002 \u9996\u5148\uFF0C\u60A8\u5FC5\u987B\u5B89\u88C5\
  \ `delve`\u3002\u60A8\u53EF\u4EE5\u901A\u8FC7\u8FD0\u884C\u4EE5\u4E0B\u547D\u4EE4\
  \u6765\u5B8C\u6210\u5B89\u88C5\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
Go 提供了一个内置的调试工具叫作 `delve`。这是一个功能齐全的调试工具，允许您逐步执行 Go 程序，检查程序变量和评估表达式。

首先，您必须安装 `delve`。您可以通过运行以下命令来完成安装：

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

现在，让我们调试一个简单的 Go 程序。考虑一个程序 `main.go`：

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

要开始调试这个程序，在项目目录的终端中执行：

```shell
dlv debug
```

此命令将在禁用优化的情况下（为了提升调试体验）编译程序，启动程序，并附加一个调试器。

一旦 `delve` 运行起来，您就进入了交互式调试器 shell。这里有一些基本命令：

- `break main.main` 在 `main` 函数处设置一个断点。
- `continue` 恢复程序执行，直到遇到断点。
- `print message` 将打印 `message` 变量的值。
- `next` 将程序执行推进到下一行。
- `quit` 退出调试器。

当触发断点并打印变量时，输出可能如下所示：

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

使用这些命令，您可以逐步检查程序，了解其行为，并查明任何问题。

## 深入了解
选择 `delve` 作为 Go 的首选调试工具，而不是像 GDB（GNU Debugger）这样的传统工具，主要是由于 Go 的执行模型和运行时的性质。GDB 最初并未考虑 Go 运行时，使得 `delve` 成为 Go 开发者更合适的选择。`Delve` 是专为 Go 设计的，为 Go 协程、通道和其他 Go 特有的结构提供了更直观的调试体验。

此外，`delve` 支持的功能远不止 GDB 在处理 Go 程序时提供的基本功能。这些功能包括但不限于：附加到正在运行的进程进行调试；条件断点；和评估可能涉及 Go 的并发原语的复杂表达式。

虽然 `delve` 是许多 Go 开发者的首选调试器，但值得注意的是，Go 工具链还包括了更轻量级的调试支持形式，如内置的 `pprof` 工具用于性能分析，以及 `trace` 工具用于并发可视化。这些工具有时可以提供更快或更高层次的途径来诊断程序性能问题或并发错误，这可能是补充的，甚至根据调试上下文而更可取。
