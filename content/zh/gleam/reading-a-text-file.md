---
title:                "Gleam: 读取文本文件"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

读取文本文件是编程世界中非常常见的任务之一。它允许您从文件中获取数据，并在程序中使用这些数据。如果您想要了解如何通过Gleam读取文本文件，那么您来对了地方！

## 如何做

在Gleam中，首先需要导入 `gleam/io` 模块来使用文件操作函数。然后，您可以使用 `io.file.read` 函数来打开并读取文本文件的内容。

```Gleam
import gleam/io { file }

pub fn main() {
  // 打开文本文件，并将内容赋值给变量 file_contents
  let file_contents = io.file.read("sample.txt")

  // 打印文件内容
  // 注意：文本文件内容将被转换为二进制数据
  // 我们可以使用模式匹配来将其转换为字符串
  // 将 file_contents 绑定到变量 contents，然后通过将其传递给 `to_string` 函数来实现。
  file_contents
  |> match {
    Ok(contents) -> contents |> to_string |> io.println
    Err(_) -> io.println("无法读取文件")
  }
  // 输出： "这是一个文本文件，包含一些示例数据"
}
```

## 深入了解

读取文本文件的过程可能会带来一些挑战。因为文本文件包含的内容可以是多种编码格式，最常见的是UTF-8和UTF-16。因此，在读取文本文件时，我们需要确保文件的编码格式与我们的程序兼容。

另一个重要的方面是异常处理。如果文件不存在或者无法读取，我们需要使用 `match` 语句来处理这些异常情况，以免程序报错。

## 参考

- [Gleam官方文档](https://gleam.run/book/getting-started.html)
- [Gleam标准库文档 - gleam/io模块](https://gleam.run/lib/gleam_io.html#modulegleam_io)
- [如何读取文本文件 - 视频教程](https://www.youtube.com/watch?v=U57vPz-Ns1E)