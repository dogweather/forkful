---
title:                "编写文本文件"
html_title:           "Gleam: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

写入文本文件是编程中常见的任务，它允许程序员将数据保存在计算机的硬盘上。这样可以方便程序随时读取这些数据，并将其应用到其他计算任务中。

## 如何：

```Gleam
file.write("hello.txt", "Hello, world!")
```

当你运行这段代码，一个新的文本文件`hello.txt`会在你的计算机上创建，内容为`Hello, world!`。使用文件写入函数，你可以将任何文本数据保存在一个文本文件中。

## 深入了解：

- 从历史角度来看，文本文件是计算机编程中最基本的文件类型之一。它可以追溯到计算机发展的早期阶段，直到今天仍然被广泛使用。
- 除了文本文件，还有其他类型的文件可以用来存储数据，比如二进制文件。二进制文件需要特殊的编码和解析方式，而文本文件则可以直接以文本形式读取和写入。
- 在Gleam中，你可以使用`file.write`函数来写入文本文件，也可以使用`file.read`函数来读取文本文件中的内容。更多关于文件操作的细节可以查看官方文档。

## 参考资料：

- Gleam官方文档：https://gleam.run
- 计算机文件类型介绍：https://www.computerhope.com/jargon/f/filetypes.htm