---
date: 2024-01-20 17:56:43.070625-07:00
description: "\u5728Rust\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\
  \u4F60\u63A5\u53D7\u7528\u6237\u5728\u542F\u52A8\u7A0B\u5E8F\u65F6\u8F93\u5165\u7684\
  \u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u5B83\u8BA9\
  \u7A0B\u5E8F\u53EF\u4EE5\u7075\u6D3B\u5730\u5E94\u5BF9\u4E0D\u540C\u7684\u7528\u9014\
  \u548C\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.538291-06:00'
model: gpt-4-1106-preview
summary: "\u5728Rust\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u4F60\
  \u63A5\u53D7\u7528\u6237\u5728\u542F\u52A8\u7A0B\u5E8F\u65F6\u8F93\u5165\u7684\u4FE1\
  \u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u5B83\u8BA9\u7A0B\
  \u5E8F\u53EF\u4EE5\u7075\u6D3B\u5730\u5E94\u5BF9\u4E0D\u540C\u7684\u7528\u9014\u548C\
  \u6570\u636E\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## 什么 & 为什么？
在Rust中读取命令行参数允许你接受用户在启动程序时输入的信息。程序员这么做是因为它让程序可以灵活地应对不同的用途和数据。

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
