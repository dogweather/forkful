---
title:                "读取命令行参数"
html_title:           "Go: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

什么是命令行参数？

命令行参数是指在运行程序时，在程序名称后面输入额外的信息。例如，在运行一个计算器程序时，我们可以输入数学表达式作为参数，这样程序就可以直接计算出结果，而无需手动输入每个数字和运算符号。

为什么要读取命令行参数？

程序员经常需要读取命令行参数，因为它可以让我们的程序变得更加灵活和自动化。通过读取命令行参数，我们可以在运行程序时传递特定的输入，从而实现不同的功能。这样一来，我们就可以根据不同的需求运行同一个程序，而不需要修改代码。

怎样读取命令行参数？

在Go 中，我们可以使用内置的 os 包来读取命令行参数。下面是一个简单的示例代码：

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // 从第二个参数开始读取
	for i, arg := range args {
		fmt.Printf("参数 %d: %s\n", i+1, arg)
	}
}
```

运行以上代码，输出将会是：

``` bash
$ go run main.go hello world

参数 1: hello
参数 2: world
```

深入了解命令行参数

命令行参数的概念实际上来源于 Unix 操作系统，它可以让用户在运行程序时通过键盘输入参数，从而实现交互式控制。除了 os 包，Go 中也有其他第三方库可以用来读取命令行参数，例如 spf13/cobra 和 urfave/cli。

除了在程序中读取命令行参数外，我们还可以通过环境变量传递参数，或者使用配置文件来存储参数。这些方法也都是很常见的命令行参数处理方式。

参考资料

- [Go语言标准库：os 包](https://go-zh.org/pkg/os/)
- [命令行参数 - 维基百科](https://zh.wikipedia.org/wiki/%E5%91%BD%E4%BB%A4%E8%A1%8C%E5%8F%83%E6%95%B0)