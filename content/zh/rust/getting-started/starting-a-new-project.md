---
date: 2024-01-20 18:04:24.731366-07:00
description: "How to \u5F00\u59CB\uFF1A \u521B\u5EFARust\u9879\u76EE\u5F88\u7B80\u5355\
  \u3002\u6253\u5F00\u7EC8\u7AEF\uFF0C\u4F7F\u7528`cargo`\u8FD9\u4E2A\u5F3A\u5927\u7684\
  \u5DE5\u5177\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.841977-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
weight: 1
---

## How to 开始：
创建Rust项目很简单。打开终端，使用`cargo`这个强大的工具。

```Rust
// 在终端里创建一个名为“hello_world”的新Rust项目
cargo new hello_world

// 进入项目文件夹
cd hello_world

// 编译并运行项目
cargo run
```
输出应该是：
```
   Compiling hello_world v0.1.0 (/path/to/hello_world)
    Finished dev [unoptimized + debuginfo] target(s) in 0.65s
     Running `target/debug/hello_world`
Hello, world!
```

## Deep Dive 深入了解：
Rust于2010年首次亮相，`cargo`自带于2015年发布的Rust 1.0中。`cargo`不仅可以创建项目，还能管理依赖和构建过程。C和C++使用`make`，Java使用`Maven`，Python用`pip`，但Rust全力支持`cargo`。它避免了编写复杂的构建脚本，简化了编译和包管理过程。启动新项目时，Cargo会自动为你生成一个合理的项目文件夹结构和一些基本的配置文件（如Cargo.toml），这让你可以立即开始编码，而不是陷入配置的泥潭。

## See Also 参考链接：
- Rust官方书籍 "The Rust Programming Language" [https://doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
- Cargo官方文档 [https://doc.rust-lang.org/cargo/](https://doc.rust-lang.org/cargo/)
- 在GitHub上查看Rust代码库 [https://github.com/rust-lang/rust](https://github.com/rust-lang/rust)
