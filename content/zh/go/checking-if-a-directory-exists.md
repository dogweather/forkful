---
title:                "Go: 检查目录是否存在"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么要检查目录是否存在？

在Go语言编程中，经常会遇到需要检查某个目录是否存在的情况。这个功能通常用于管理文件系统和操作文件。通过检查目录是否存在，我们可以确保代码在运行时不会出现意料之外的错误。接下来，我们将介绍如何使用Go语言来检查目录是否存在以及如何处理它。

## 如何检查目录是否存在

在Go语言中，通过使用`os.Stat()`函数来检查目录是否存在。这个函数接受一个`string`类型的参数，该参数为待检查的目录路径。如果该路径指向一个已存在的目录，函数将返回一个`FileInfo`类型的值。否则，将返回一个`error`类型的值，指示目录不存在。下面是一个简单的代码示例：

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // 假设我们要检查的目录名为"test"
    dir := "test" 

    // 使用os.Stat()函数检查目录是否存在
    _, err := os.Stat(dir)

    // 如果返回的error不为空，说明目录不存在
    if err != nil {
        fmt.Println("目录不存在")
    } else {
        fmt.Println("目录存在")
    }
}
```

上述代码中，我们使用`os.Stat()`函数来检查`test`目录是否存在。如果目录存在，则会通过`fmt.Println()`函数输出“目录存在”。否则，会输出“目录不存在”。这是最基本的检查目录存在的方法，在实际的应用中，我们还可以对返回的`error`进行更加细致的处理。

## 深入了解

在深入理解检查目录是否存在的过程中，我们还需要了解`FileInfo`类型和`os.Stat()`函数的工作原理。在Go语言中，`FileInfo`类型表示一个文件或目录的元信息。我们可以通过调用`os.Stat()`函数来获取该类型的值，从而获取关于目录的信息。如果调用`os.Stat()`函数返回的error为空，则说明目录存在，否则返回的error将指示该目录不存在。

## 查看其他资源

- [Go语言文档 - os.Stat()函数](https://golang.org/pkg/os/#Stat)
- [Go语言文档 - os.FileInfo类型](https://golang.org/pkg/os/#FileInfo)
- [Go语言教程 - 如何检查文件或目录是否存在](https://www.runoob.com/go/go-check-file-or-directory-exists.html)