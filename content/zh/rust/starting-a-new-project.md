---
title:                "开始一个新项目"
date:                  2024-01-20T18:04:24.731366-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? 什么是新项目？以及为什么要创建？
新项目就是你准备开发的软件的起点。程序员创建新项目是为了组织代码、测试和文档，从零开始构建想法。

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