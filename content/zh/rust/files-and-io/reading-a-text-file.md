---
title:                "阅读文本文件"
aliases:
- /zh/rust/reading-a-text-file.md
date:                  2024-01-20T17:55:11.337699-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/reading-a-text-file.md"
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
