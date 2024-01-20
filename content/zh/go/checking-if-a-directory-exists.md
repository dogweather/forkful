---
title:                "检查目录是否存在"
html_title:           "Go: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么和为什么？

检查目录是否存在是一种常用的编程技巧，它用于确认给定的文件夹在编程环境中是否确实可用。它对于资源管理和预防运行时错误至关重要。

## 如何做：

在Go中检查一个目录是否已经存在可以使用`os`包的`IsExist`和`os.IsNotExist`函数。请看以下例子：

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Stat("/path/to/directory")

    if os.IsNotExist(err) {
        fmt.Println("指定的目录不存在")
    } else {
        fmt.Println("指定的目录存在")
    }
}
```

如果路径存在，程序将打印"指定的目录存在"，否则打印"指定的目录不存在"。

## 深入研究

不同的编程语言，包括早期Go的版本全部有不同的方式检查目录是否存在。`os`包中的`IsExist`和`os.IsNotExist`方法提供了直接并常用的方式进行检查。

尽管Go内建的检查方法已经非常方便，但也有些其他的替代方案，例如使用`os.Open`函数打开目录并检查返回的错误值。

```Go
_, err := os.Open("/path/to/directory") 
if err != nil {
    fmt.Println("指定的目录不存在") 
} else {
    fmt.Println("指定的目录存在")
}
```

然后它跟前者的区别在于`os.Open`尝试打开该目录，如果目录不存在或者无权限访问都会返回错误。

## 另请参阅：

Go编程手册：https://tour.golang.org/welcome/1

Go os package 文档: https://golang.org/pkg/os/

如何在Go中检查文件是否存在：https://www.socketloop.com/tutorials/golang-check-if-a-file-exist-or-not