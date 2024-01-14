---
title:    "Go: 写入标准错误"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么写标准错误？

在Go编程中，写入标准错误是一种常见的调试技巧。通过将错误信息输出到标准错误流，我们可以更方便地追踪程序的错误。这可以帮助我们更快地定位和修复代码的问题，提高程序的质量。

# 如何写入标准错误？

使用Go程序语言，我们可以使用标准库中的"fmt"包来写入标准错误。下面是一个简单的示例代码：

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Open("nonexistent_file.txt") // 尝试打开不存在的文件
    if err != nil {
        fmt.Fprintln(os.Stderr, "文件打开错误:", err) // 将错误信息输出到标准错误流
    }
}
```

输出结果将会是：

```Go
文件打开错误: open nonexistent_file.txt: no such file or directory
```

如此简单的一行代码，就可以将错误信息输出到标准错误流中。这样，我们就可以通过查看标准错误流来得知出错的具体信息，从而更快地解决问题。

# 深入了解

标准错误是一种特殊的文件流，它与标准输出流（通常用于打印普通日志信息）分开，可以更清晰地区分程序的不同输出。在Go中，我们可以通过调用`fmt.Fprintln()`函数并传入`os.Stderr`参数来将信息输出到标准错误流中。这个函数的原型如下：

```Go
func Fprintln(w io.Writer, a ...interface{}) (n int, err error)
```

其中，第一个参数`w`表示输出流的目标位置，第二个参数`a`是需要输出的内容。通过这个函数，我们可以更灵活地控制输出的位置和格式。

# 参考链接

- [Go官方文档 - fmt包](https://golang.org/pkg/fmt/)
- [Go官方文档 - os包](https://golang.org/pkg/os/)
- [Go中的标准错误输出](https://dev.to/koddr/go-how-to-use-stderror-stream-42nb) (英文)
- [Gopher中文社区 - 使用标准错误输出](https://www.golangtc.com/t/5a29d262320b522c8e263c57) (中文) 

# 参考链接