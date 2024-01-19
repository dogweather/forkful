---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么与为什么？
读取文本文件是编程中常见的一种操作，它指的是程序从硬盘或内存中读取并解析一个特定的文本文件。程序员这么做是为了获取、使用或分析文件中的数据。

## 如何操作：
```Gleam 
案例1：读取文件所有内容
import gleam/io.{assert, console, file}

pub fn read_file() {
  let path = "_data/hello.txt"
  case file.read(path) {
    Ok(data) -> console.println(data)
    Error(err) -> console.println(err)
  }
}
```  
输出：
```
Hello, Gleam!
```

案例2：逐行读取文件
```Gleam
import gleam/io.{assert, console, file}

pub fn read_file_line_by_line() {
  let path = "_data/hello.txt"
  let lines = file.read_lines(path)
  
  case lines {
    Ok(lines) -> lines.iter().for_each(|line| console.println(line))
    Error(err) -> console.println(err)
  }
}
```
输出：
```
Hello,
Gleam!
```

## 深入剖析
历史背景：当计算机初诞生的时候，遗憾的是，硬盘是很昂贵、并且容量十分有限的。因此只能通过将数据分散存储来提高存储效率。这就是为什么文件有了行的概念，每一行数据都可以分别读取。

替代方案：有时，数据太大，无法一次性读入内存，那么我们可以使用流式读取，从而提高内存效率。另一个方案是数据结构存储，比如数据库、键值对等。

实现细节：读文件操作通常分为打开文件、读取文件、关闭文件三个步骤。在某些语言中(例如 C)，你需要手动关闭文件。而在 Gleam 中，这一切工作都由编译器自动完成。

## 参见
1. 官方文档 - [Gleam 文件操作文档](https://gleam.run/book/tour/files.html)
2. 相关课程 - [Gleam 语言学习教程](https://gleam.run/learning/)
3. 参考代码 - [GitHub 上的 Gleam 代码例子](https://github.com/gleam-lang/example/)
4. 讨论区 - [Gleam 论坛](https://community.gleam.run/)