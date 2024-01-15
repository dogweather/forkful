---
title:                "请转到标准错误"
html_title:           "Go: 请转到标准错误"
simple_title:         "请转到标准错误"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写标准错误

大家好！今天我们来聊聊为什么我们会想要写入标准错误。首先，标准错误是计算机程序中很常见的一个概念。它是指程序出错时打印出的信息。这对于调试代码和记录错误都非常有用。当我们编写代码时，经常会遇到各种各样的错误，而写入标准错误可以帮助我们快速定位并解决问题。

## 如何进行

在Go中，我们可以使用`os.Stderr`来访问标准错误并将信息写入。下面是一个简单的示例代码：

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintln(os.Stderr, "这是一条写入标准错误的信息")
}
```

上面的代码中，`fmt.Fprintln`函数允许我们将指定的信息写入到指定的输出流，这里我们选择了标准错误流`os.Stderr`。运行以上代码，我们可以得到如下输出：

```bash
这是一条写入标准错误的信息
```

## 深入了解

除了上面提到的使用`fmt.Fprintln`函数外，我们还可以使用`fmt.Fprintf`和`fmt.Fprint`函数来写入标准错误。它们的用法与`fmt.Println`、`fmt.Printf`和`fmt.Print`函数类似，只是多了一个写入流的参数。此外，我们还可以使用`os.Stderr.WriteString`函数来直接写入字符串到标准错误。需要注意的是，当我们将信息写入标准错误时，它会立即打印出来，而不需要像使用`fmt.Println`函数一样需要在最后加上换行符`\n`。

## 更多资源

- [Go官方文档](https://golang.org/doc/)
- [Go标准库文档](https://golang.org/pkg/)
- [关于标准错误的更多知识](https://en.wikipedia.org/wiki/Standard_error_(stream))