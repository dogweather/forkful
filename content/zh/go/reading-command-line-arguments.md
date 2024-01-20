---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 一起学Go利用命令行参数的实践使用 

## 什么和为什么?
命令行参数是启动程序时输入给程序的信息。为什么需要命令行参数呢？它们让程序更灵活，可以在不修改代码的情况下调整程序的行为。

## 如何做
在Go中，获取命令行参数的主要手段是内置的`os`包。看下面这个简单的例子：

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println(args)
}
```

如果你把这个程序命名为`args.go`，然后通过`go run args.go hello world`来运行，输出将会是：

```Go
[./args hello world]
```

可以看出，`os.Args`是一个切片，它的第一个元素是程序的路径，后面跟的元素是命令行输入的参数。

## 深入探讨
实际上，在Unix的传统中，命令行参数的使用有很长的历史，这种设计的根本目的就是为了让程序在不修改代码的情况下提供不同的行为。

在Go中，除了`os.Args`，还有`flag`包可以提供更强大的命令行参数处理能力，比如解析带有命令选项的命令行参数。

命令行参数是存储在进程启动时操作系统提供的程序参数区域，通过程序库函数获取和使用。

## 参看
如果你想了解更多关于命令行参数的信息，这里有几个不错的地方：

* [Go官方文档](https://pkg.go.dev/os) ： 帮助你更深入地理解`os.Args`。
* [Go flag包](https://pkg.go.dev/flag)：一个用于处理命令行选项的强大工具。

祝你学习愉快！