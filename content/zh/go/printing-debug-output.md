---
title:                "Go: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么要打印调试输出

你是否遇到过在编写Go程序时遇到一些难以定位的bug？打印调试输出可以帮助你更快地定位和解决这些问题。

## 如何打印调试输出

要打印调试输出，你可以使用内置的fmt包中的Print和Printf函数。例如：

```
package main

import "fmt"

func main() {
    num := 10
    fmt.Print("这是一个数字：", num) // 打印一行调试输出
    fmt.Printf("这是一个数字：%d\n", num) // 格式化打印调试输出
}
```

运行上述代码，你将会得到如下的输出：

```
这是一个数字：10
这是一个数字：10
```

## 深入了解打印调试输出

除了使用Print和Printf函数之外，我们还可以使用Sprint和Sprintf函数来将调试输出保存为一个字符串，方便后续处理。另外，fmt包中还有一些其他的打印函数，如Println和Fprintf，也可以用于打印调试输出。你可以查阅Go官方文档来了解更多关于fmt包的用法和细节。

## 参考链接

- [Go官方文档：fmt包](https://golang.org/pkg/fmt/)
- [Go中文网：fmt包](https://studygolang.com/pkgdoc)
- [How To Debug In Go](https://blog.mailgun.com/how-to-debug-in-go/) (英文)