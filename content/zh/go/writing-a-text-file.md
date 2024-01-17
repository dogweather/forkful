---
title:                "编写文本文件"
html_title:           "Go: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 这是什么 & 为什么要做？
写文本文件是指使用编程语言向计算机中保存文本数据。程序员常常这样做，是为了保存程序运行过程中产生的信息，以及作为日志记录和数据存储的方式。

## 如何：
```Go
// 导入标准库中的 "os" 模块
import "os"

// 创建一个新的文本文件
file, err := os.Create("example.txt")
if err != nil {
    panic(err)
}

// 写入文本数据
file.WriteString("这是一个例子文本文件。")

// 关闭文件
err = file.Close()
if err != nil {
    panic(err)
}
```
输出结果：创建名为 "example.txt" 的文本文件，并将 "这是一个例子文本文件。" 写入到文件中。

## 深入了解：
写文本文件的历史可以追溯到计算机发展的早期，它的一个替代方法是使用数据库存储数据。写文本文件的过程可以通过使用不同的方法来实现，例如在 Go 中使用 "fmt" 模块将数据写入文件，但是这种方法通常会消耗更多的处理器时间。

## 参考链接：
- https://golang.org/pkg/os/#Create
- https://golang.org/pkg/fmt/#Fprintf