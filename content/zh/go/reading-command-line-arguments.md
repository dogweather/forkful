---
title:    "Go: 读取命令行参数"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#为什么：

当你在编写Go程序时，有时候你想让程序能够接收用户输入的一些参数。这些参数可以通过命令行来传递。通过学习如何读取命令行参数，你可以让你的程序更加灵活和可定制。

##如何做：

在Go语言中，你可以使用内置的`os`包来读取命令行参数。首先，你需要使用`os.Args`函数来获取所有传递给程序的参数。然后，你可以使用索引来访问每个参数，索引从0开始，包括程序本身的名称。下面是一个简单的示例：

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  args := os.Args
  fmt.Println("程序名称：", args[0])
  fmt.Println("参数数量：", len(args)-1)
  fmt.Println("参数列表：", args[1:])
}
```

运行结果（假设程序名称为`program.go`）：

```bash
$ go run program.go 1 2 3
程序名称： program.go
参数数量： 3
参数列表： [1 2 3]
```

##深入了解：

除了使用索引，你也可以使用`flag`包来读取命令行参数。`flag`包提供了更加直观和可定制的方法来读取参数。下面是一个使用`flag`包的例子：

```Go
package main

import (
  "flag"
  "fmt"
)

func main() {
  // 定义一个参数，名称为lang，默认值为"en"
  lang := flag.String("lang", "en", "语言选择")
  
  // 解析命令行参数
  flag.Parse()
  
  fmt.Println("你选择的语言是：", *lang)
}
```

运行结果（假设程序名称为`program.go`）：

```bash
$ go run program.go -lang cn
你选择的语言是： cn
```

你也可以使用`flag`包来定义更多的参数，比如整型参数、布尔型参数等。详细的说明可以参考官方文档。

#参考链接：

- [Go语言标准库-os包](https://studygolang.com/pkgdoc)
- [Go语言标准库-flag包](https://studygolang.com/pkgdoc)
- [Learning Go 第三版（中文版）-命令行参数](https://books.studygolang.com/Go-study-index/ch01s03.html#cli)

#更多阅读：

如果你想深入了解Go语言的命令行参数读取原理，可以查看`flag`包的源码。另外，你也可以尝试使用其他第三方库来读取命令行参数，比如`Go-flags`和`Kingpin`等。