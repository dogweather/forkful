---
title:                "Go: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

虽然写文本文件可能并不是编程中最激动人心的部分，但它却是非常重要的。通过编写文本文件，您可以保存和共享数据，而不是让它们丢失在程序结束后。它也可以让您更方便地阅读和编辑数据，而无需运行整个程序。

## 如何操作

在Go语言中，编写文本文件非常容易。首先，您需要导入 "os" 和 "io/ioutil" 包。然后，使用 "os.Create()" 函数来创建一个文件，并指定文件名和权限。接着，使用 "WriteString()" 函数来写入内容，并使用 "Close()" 函数来保存和关闭文件。最后，使用 "fmt.Println()" 函数打印操作完成的提示信息。

```Go
import (
  "os"
  "io/ioutil"
)

func main() {
  file, err := os.Create("sample.txt") // 创建文件
  if err != nil {
    fmt.Println("创建文件错误：", err)
    return
  }
  defer file.Close()

  content := "这是一个文本文件的示例。" // 写入的内容
  _, err = file.WriteString(content) // 写入文件
  if err != nil {
    fmt.Println("写入文件错误：", err)
    return
  }

  file.Close()
  fmt.Println("文本文件写入成功！")
}
```

运行以上程序后，在同一目录下就会生成一个名为 "sample.txt" 的文本文件，其中包含 "这是一个文本文件的示例。" 这一行内容。如果您想要写入多行内容，可以在每行末尾添加一个换行符 "\n"。您也可以使用 "file.WriteString()" 函数多次写入内容，每次写入一行。最后，使用 "file.Close()" 函数来保存和关闭文件。

## 深入了解

除了使用 "WriteString()" 函数来写入文本文件外，您也可以使用 "file.Write()" 函数来写入字节切片，以及 "file.WriteString()" 和 "file.WriteByte()" 函数来写入单个字符。此外，如果您想要在写入内容时追加到文本文件末尾，可以使用 "file.WriteString()" 和 "file.Write()" 函数的前缀 "file.Seek(0, io.SeekEnd)" 来实现。

## 参考链接

- [编写文件 - Go语言标准库文档](https://golang.org/pkg/os/#Create)
- [写入文本文件 - Go语言开发者指南](https://tour.golang.org/methods/16)
- [文本文件操作示例 - 代码行数](https://www.codewars.com/kata/writing-to-a-text-file-with-golang/discuss/go)