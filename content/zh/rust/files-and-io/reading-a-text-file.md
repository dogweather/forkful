---
date: 2024-01-20 17:55:11.337699-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u5B58\u50A8\u5728\
  \u6587\u4EF6\u4E2D\u7684\u6570\u636E\u52A0\u8F7D\u5230\u7A0B\u5E8F\u91CC\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u5904\u7406\u6216\u8005\
  \u5206\u6790\u8FD9\u4E9B\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.540585-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u5B58\u50A8\u5728\
  \u6587\u4EF6\u4E2D\u7684\u6570\u636E\u52A0\u8F7D\u5230\u7A0B\u5E8F\u91CC\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u5904\u7406\u6216\u8005\
  \u5206\u6790\u8FD9\u4E9B\u6570\u636E\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
读取文本文件就是把存储在文件中的数据加载到程序里。程序员这么做主要是为了处理或者分析这些数据。

## How to: 怎么做：
```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("hello.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("File Contents:\n{}", contents);
    Ok(())
}
```
输出样例：
```
File Contents:
Hello, world!
```

## Deep Dive 深入探讨：
Rust 从诞生之初就有了处理文件I/O的能力。上述代码是读取文本文件的基本方法，但是历史上我们也有其他的选择，比如使用`std::fs::read_to_string`函数。`File::open` 和 `read_to_string`操作简单，适用小文件。大文件则考虑按行读取或者块读取以节省内存。

实现详情方面，Rust 保证了类型安全和内存安全，就是说读文件时，如果出错了，程序不会崩溃，它会返回一个`Result`类型让你处理错误。

## See Also 相关资源：
- Rust 书[官方指南]: https://doc.rust-lang.org/stable/book/ch12-02-reading-a-file.html
- [std::fs]: https://doc.rust-lang.org/stable/std/fs/index.html
- [std::io]: https://doc.rust-lang.org/stable/std/io/index.html
- [The Rust Programming Language 练习书]: https://github.com/rust-lang/rustlings/
