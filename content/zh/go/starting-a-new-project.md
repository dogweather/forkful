---
title:                "开始一个新项目"
html_title:           "Go: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 为什么

开始一个新项目可能是因为想要学习一门新语言、尝试一种新的编程风格、或者是为了解决一个特定的问题。无论是什么原因，Go语言都是一个非常适合的选择。

## 如何开始

首先，你需要安装Go编译器。在[官方网站](https://golang.org/dl/)上可以找到适用于不同操作系统的安装程序。

接下来，你可以使用任何文本编辑器创建一个新的Go文件，后缀名为`.go`。比如，我们可以创建一个名为`hello.go`的文件，里面的内容如下：

```Go
package main

import "fmt"

func main() {
    fmt.Println("你好，世界！")
}
```

然后，在终端中运行以下命令，编译并运行这个文件：

```Go
go run hello.go
```

你会看到控制台输出了`你好，世界！`，这就是我们的第一个Go程序。

## 深入了解

如果你想要深入学习Go语言，可以去阅读官方的[语言文档](https://golang.org/doc/)。文档中包含了关于语法、标准库等方面的详细说明。同时，你也可以参考[Go语言之旅](https://tour.golang.org/welcome/1)来学习一些实际的例子。

此外，如果你希望使用Go来开发Web应用，可以学习[官方的Web编程指南](https://golang.org/doc/articles/wiki/)，或者参考[这个网站](https://gowebexamples.com/)上的实际示例。

## 参考链接

- [Go语言官方网站](https://golang.org/)
- [Go语言文档](https://golang.org/doc/)
- [Go语言之旅](https://tour.golang.org/welcome/1)
- [官方的Web编程指南](https://golang.org/doc/articles/wiki/)
- [GoWebExamples网站](https://gowebexamples.com/)