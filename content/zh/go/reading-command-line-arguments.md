---
title:                "Go: 读取命令行参数"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么：读取命令行参数是Go语言中非常重要的一个功能，它可以让我们的程序可以通过用户输入来改变其行为。通过这篇博文，你将了解到如何使用Go语言来读取命令行参数，并且深入了解这个功能是如何工作的。

如何：通过下面的 ```Go ... ``` 代码块，你将学习如何在Go语言中读取命令行参数，并且得到一个样例输出。这将帮助你更好地理解这个功能的用法。

```Go
package main

import "fmt"
import "os"

func main() {
    // 通过 os.Args 变量获取命令行参数
    args := os.Args
    // 输出命令行参数个数
    fmt.Println("Number of arguments:", len(args))

    // 循环输出每个命令行参数
    for i := 0; i < len(args); i++ {
        fmt.Println("Argument", i+1, ":", args[i])
    }
}
```

运行该程序，输入命令行参数，你将得到以下输出：

```
$ go run main.go arg1 arg2 arg3

Number of arguments: 4
Argument 1: main.go
Argument 2: arg1
Argument 3: arg2
Argument 4: arg3
```

深入了解：除了通过循环方式来获取命令行参数，我们还可以使用 `flag` 包来实现。这个包提供了更多的功能，比如可以定义各种类型的命令行参数，并且可以为它们添加默认值、帮助信息等。如果你想要更加灵活地读取命令行参数，可以尝试使用 `flag` 包来完成。

另外，你可能会发现，在运行该程序时，命令行参数会被包括在程序名之后，即第一个参数是程序本身的文件名。如果你想要忽略程序名，只获取后面的参数，可以通过 `os.Args[1:]` 来获取一个不包含程序名的参数列表。

参考链接：

- [Go语言文档 - 命令行参数](https://golang.google.cn/pkg/os/#Args)
- [Go语言文档 - flag包](https://golang.google.cn/pkg/flag)
- [Go语言实战 - 命令行参数](https://github.com/golang/go/wiki/CommandLineParameters)

另请参阅：

如果你对命令行参数的读取还有其他疑问，可以参考上面的参考链接，或者查看Go语言官方文档中的更多内容。Go语言社区也有许多其他的资源，可以帮助你更好地使用命令行参数这个功能，比如 [Go语言中文网](https://studygolang.com/) 或者 [Go语言中文社区](https://github.com/golang-china/golang.org) 等。祝你在学习Go语言的过程中，获得更多的知识与技能！