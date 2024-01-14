---
title:                "Go: 向标准错误写入"
simple_title:         "向标准错误写入"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：写入标准错误

通常情况下，我们使用标准输出来显示程序的结果。但是有时候，我们需要将一些错误信息发送给用户，这时候就需要用到标准错误输出。通过将错误信息写入标准错误，我们可以让用户更清楚地知道程序发生了什么问题，帮助他们更好地理解程序运行过程。

如何做：在Go中编写标准错误

要将错误信息写入标准错误，我们可以使用标准库中的方法`fmt.Fprintf`。这个方法的第一个参数是一个`io.Writer`接口，其中就包含了标准错误输出流。下面是一个简单的示例代码，在使用`fmt.Fprintf`将错误信息写入标准错误后，程序会终止运行并输出错误信息。

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "文件打开失败：%v\n", err)
		os.Exit(1)
	}
	defer file.Close()
	// do something with the file
}
```

输出：

```
文件打开失败：open test.txt: no such file or directory
```

深入了解：写入标准错误的更多方法

除了`fmt.Fprintf`之外，Go标准库还提供了其他方法用于写入标准错误，比如`log`包中的方法和`os.Stderr`变量。此外，我们也可以自定义一个`Writer`类型的对象来实现将错误信息写入标准错误的功能。具体的实现方法可以参考Go官方文档和其他教程。

请参见：

- [Go文档 - fmt.Fprintf](https://golang.org/pkg/fmt/#Fprintf)
- [Go文档 - log](https://golang.org/pkg/log/)
- [Go语言中文网 - 错误处理](https://studygolang.com/articles/3806)
- [CSDN - Go语言标准库中的Fprint之fmt包详解](https://blog.csdn.net/cywosp/article/details/40853345)

请参阅：

- 阅读更多有关标准错误的内容
- 学习其他有用的Go编程技巧