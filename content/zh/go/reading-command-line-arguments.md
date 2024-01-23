---
title:                "读取命令行参数"
date:                  2024-01-20T17:56:06.107719-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
读取命令行参数让你的Go程序能接受从外部输入的信息。程序员这么做是为了让程序更灵活，可以根据用户的不同输入执行不同操作。

## How to: (如何操作：)
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // Skip the program name
	fmt.Println("Command Line Arguments:", args)
}
```

运行程序 `go run main.go arg1 arg2` 的输出：
```
Command Line Arguments: [arg1 arg2]
```

## Deep Dive (深入了解)
命令行参数是一种古老的传参方式，早在图形界面出现前就被用来和程序交互了。在Go中，`os.Args` 提供了一个简单直接的方式来获取这些参数。它是一个字符串切片，`os.Args[0]` 是程序本身的路径，所以我们通常从 `os.Args[1]` 开始读取真正的输入参数。除了 `os` 包，还有 `flag` 包可用于解析命令行参数，它提供了更复杂的功能，比如默认值和参数类型转换。`os.Args` 对于简单场景足够用，而 `flag` 包适合需要更丰富命令行选项的程序。

## See Also (另请参阅)
- [Go by Example: Command-Line Arguments](https://gobyexample.com/command-line-arguments)
- [Go标准库文档：os包](https://pkg.go.dev/os)
- [Go标准库文档：flag包](https://pkg.go.dev/flag)
