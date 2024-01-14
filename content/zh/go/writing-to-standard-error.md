---
title:                "Go: 标准错误的编写"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么

为什么会有人想要把一些信息写入标准错误流（stderr）呢？正如我们所知，标准输入流（stdin）和标准输出流（stdout）分别用于接收用户的输入和输出程序的结果。然而，在某些情况下，我们需要将一些错误信息显示给用户，这时就需要用到标准错误流了。

# 如何操作

如果你想要将信息显示给用户，你可以使用log包中的Print()或Printf()函数来进行输出，它们都会将信息写入标准错误流。下面是一个使用Print()函数的示例：

```Go
package main

import "log"

func main() {
	log.Print("这是一个错误信息。")
}
```

运行这段代码，你会发现错误信息会被打印出来。如果你想要将错误信息和其他信息一起打印出来，可以使用Printf()函数来格式化输出。下面是一个使用Printf()函数的示例：

```Go
package main

import "log"

func main() {
	name := "小明"
	log.Printf("欢迎%s登录。", name)
	log.Print("这是一个错误信息。")
}
```

运行这段代码，你会看到控制台打印出了欢迎信息和错误信息。上面的示例都是将信息打印到控制台，如果你想要将信息写入一个文件，可以使用log包中的SetOutput()函数来设置输出位置。下面是一个将信息写入文件的示例：

```Go
package main

import (
	"log"
	"os"
)

func main() {
	f, err := os.OpenFile("log.txt", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	log.SetOutput(f)
	log.Print("这是一个写入文件的错误信息。")
}
```

运行这段代码，你会发现在目录下生成了一个名为log.txt的文件，其中记录了错误信息。除了Print()和Printf()函数外，log包中还有其他用于输出信息的函数，你可以根据需要选择合适的函数来使用。

# 深入了解

虽然在日常的编程中，我们经常会使用标准错误流来输出错误信息，但它其实是一个非常简单的概念。标准错误流是一个用于输出错误信息的流，在操作系统中通常被定义为文件描述符2。在Go语言中，我们可以通过os.Stderr变量来访问标准错误流，它是一个指向标准错误流文件描述符的File类型变量。

# 参考链接

- [log包文档](https://golang.org/pkg/log/)
- [标准流（stdin、stdout、stderr）的概念](https://zh.wikipedia.org/wiki/%E6%A0%87%E5%87%86%E6%B5%81)
- [使用os包操作文件](https://golang.org/pkg/os/)