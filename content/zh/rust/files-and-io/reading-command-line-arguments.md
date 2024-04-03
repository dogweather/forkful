---
date: 2024-01-20 17:56:43.070625-07:00
description: "\u600E\u4E48\u505A\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.538291-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## 怎么做：
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("接收到的命令行参数有：");
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

如果运行程序: `cargo run 这是 测试`，输出将会是：
```
接收到的命令行参数有：
target/debug/programname
这是
测试
```

## 深入探究
读取命令行参数的实践很久远。在早期，程序通常是命令行首发，图形界面是后来才出现的。在Rust中，`std::env::args` 是读取参数的标准方法，它返回一个迭代器。Rust还有其他方式，如使用`clap`或`structopt`库，可以更方便地解析复杂的命令行参数。

命令行参数的索引从0开始，其中第一个参数一般是程序本身的路径。随后的参数是用户提供的，可以用它们指定配置选项、传递文件路径或其他数据。

在Rust的标准库之外，`clap`和`structopt`采用了声明式的宏来简化参数解析和验证的流程。如果你需要处理更复杂的命令行参数，可能会想要考虑使用它们。但对于简单场景，标准库已经足够了。

## 看看这个：
- Rust `std::env` 模块文档：https://doc.rust-lang.org/std/env/
- `clap`库的文档：https://docs.rs/clap/
- `structopt`库的文档：https://docs.rs/structopt/
- Rust 书籍中关于命令行程序的章节：https://doc.rust-lang.org/book/ch12-00-an-io-project.html
