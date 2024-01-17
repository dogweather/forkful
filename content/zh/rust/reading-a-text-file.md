---
title:                "读取文本文件"
html_title:           "Rust: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
阅读文本文件是指从计算机中读取文本内容的过程。程序员经常需要读取文本文件来获得必要的数据，例如配置文件、日志文件等等。

## 如何：
```
Rust code block
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // 打开文件，读取内容到变量
    let mut file = File::open("example.txt").expect("无法打开文件");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("无法读取文件内容");

    // 打印文件内容
    println!("{}", contents);
}
```
输出内容：
```
Hello, world!
This is an example text file.
```

## 深入了解：
阅读文本文件这个概念是非常基础的，但却是非常重要的。它可以追溯到早期的计算机操作系统，但是随着技术的发展，现在有许多替代方法，例如使用数据库等。在Rust中，可以使用标准库中的 `std::fs` 和 `std::io` 来读取文本文件。

## 查看更多：
- Rust标准库文档：https://doc.rust-lang.org/std/fs/index.html
- Rust语言官方网站：https://www.rust-lang.org