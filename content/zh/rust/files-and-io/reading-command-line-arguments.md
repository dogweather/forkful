---
date: 2024-01-20 17:56:43.070625-07:00
description: "\u600E\u4E48\u505A\uFF1A \u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u7684\
  \u5B9E\u8DF5\u5F88\u4E45\u8FDC\u3002\u5728\u65E9\u671F\uFF0C\u7A0B\u5E8F\u901A\u5E38\
  \u662F\u547D\u4EE4\u884C\u9996\u53D1\uFF0C\u56FE\u5F62\u754C\u9762\u662F\u540E\u6765\
  \u624D\u51FA\u73B0\u7684\u3002\u5728Rust\u4E2D\uFF0C`std::env::args` \u662F\u8BFB\
  \u53D6\u53C2\u6570\u7684\u6807\u51C6\u65B9\u6CD5\uFF0C\u5B83\u8FD4\u56DE\u4E00\u4E2A\
  \u8FED\u4EE3\u5668\u3002Rust\u8FD8\u6709\u5176\u4ED6\u65B9\u5F0F\uFF0C\u5982\u4F7F\
  \u7528`clap`\u6216`structopt`\u5E93\uFF0C\u53EF\u4EE5\u66F4\u65B9\u4FBF\u5730\u89E3\
  \u6790\u590D\u6742\u7684\u547D\u4EE4\u884C\u53C2\u6570\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.742645-06:00'
model: gpt-4-1106-preview
summary: "\u547D\u4EE4\u884C\u53C2\u6570\u7684\u7D22\u5F15\u4ECE0\u5F00\u59CB\uFF0C\
  \u5176\u4E2D\u7B2C\u4E00\u4E2A\u53C2\u6570\u4E00\u822C\u662F\u7A0B\u5E8F\u672C\u8EAB\
  \u7684\u8DEF\u5F84\u3002\u968F\u540E\u7684\u53C2\u6570\u662F\u7528\u6237\u63D0\u4F9B\
  \u7684\uFF0C\u53EF\u4EE5\u7528\u5B83\u4EEC\u6307\u5B9A\u914D\u7F6E\u9009\u9879\u3001\
  \u4F20\u9012\u6587\u4EF6\u8DEF\u5F84\u6216\u5176\u4ED6\u6570\u636E\u3002"
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
