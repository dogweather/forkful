---
title:    "Gleam: 读取文本文件"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

Why (为什么）：

在编程中，读取文本文件是一个非常常见的任务。它允许你从文件中提取数据并将其用于你的程序中。读取文本文件可以帮助你更轻松地处理数据，并使你的程序更具灵活性。 

How To（如何）：

```Gleam
import gleam/file

// 打开一个文本文件并将其存储为一个变量
file := file.open("example.txt")

// 使用read_all函数读取整个文件的内容
content := file.read_all()

// 使用split_lines函数将文本按行拆分为一个字符串列表
lines := content.split_lines()

// 使用for循环遍历所有行并打印每一行的内容
for line in lines {
  println(line)
}
```
例子输入：

文本文件内容为：

```
Hello
World
```

例子输出：

```
Hello
World
```

Deep Dive（深入研究）：

除了使用read_all和split_lines函数，Gleam还提供了其他功能来读取文本文件。比如，你可以使用file.read_line函数来逐行读取文本，并使用file.read_bytes函数来读取文件的指定字节数。此外，Gleam还提供了一些处理Unicode文本的功能，如file.read_utf8和file.read_utf16。通过深入学习这些函数，你可以更有效地处理文本文件。

See Also（相关阅读）：

- Gleam文档：https://gleam.run/core/file.html
- 了解更多Gleam编程语言：https://gleam.run/
- 学习如何使用Gleam处理文件： https://gleam.run/tutorials/file-handling.html