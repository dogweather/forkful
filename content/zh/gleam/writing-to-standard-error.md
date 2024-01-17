---
title:                "写入标准错误"
html_title:           "Gleam: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

### 什么是 & 为什么要： 
将错误信息写入标准错误是指程序员将错误信息输出到用户界面的一种方法。这样做可以让用户更容易地发现错误并解决问题，同时也可以帮助开发人员调试程序。

### 如何： 
以下是在Gleam中使用标准错误输出的示例代码和输出结果： 
```Gleam
import gleam/io 
io.error("这是一个错误信息") 
```
输出结果： 
`这是一个错误信息`

### 深入了解： 
在过去，程序员通常会将错误信息输出到标准输出，但这样做可能会导致用户错过重要的错误信息。因此，现在通常会选择将错误信息输出到标准错误，这样可以让用户更容易地发现并解决问题。如果你想要在编写Gleam程序时更改错误输出的默认行为，你可以使用Gleam的命令行参数来将错误信息输出到标准输出。

### 相关链接： 
了解更多关于标准错误输出和Gleam的信息，请查看以下链接： 
- 官方文档：https://gleam.run/documentation/stdlib/io.html#error 
- Stack Overflow上的讨论： https://stackoverflow.com/questions/18122264/how-to-print-to-stderr-in-gleam 
- Gleam的GitHub仓库： https://github.com/gleam-lang/gleam