---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？

启动一个新项目意味着创建一个全新的编程工作环境。程序员之所以需要新项目，是因为他们希望在清晰、独立的环境中开发特定的应用。

## 如何操作：

在Rust中创建新项目非常简单。您可以使用Cargo（Rust的包管理器）来做到这一点。下面是该过程的代码和实例输出：

```Rust
// 运行这条命令来创建一个新项目
cargo new my_project

// 这会在您的项目目录生成以下结构
my_project/
    ├── Cargo.toml
    └── src
        └── main.rs
```

`Cargo.toml`是一个包配置文件，`src/main.rs` 是您的程序主入口。

## 深入探讨：

在过去，不同的编程语言都有其生成新项目的方式，Rust应运而生，旨在提供一种更快且更安全的方式。对于其他替代方案，如果您是C++或Java用户，可能会找到像CMake这样的工具更熟悉。而对于Python或JS开发者，可能更惯用virtualenv或npm init。对于Rust的实现细节，`cargo new`生成一个基本的项目结构，拥有最少的项目元数据和一个简单的“Hello, world!”编程样本。

## 另请参阅：

- [Cargo指南](https://doc.rust-lang.org/stable/cargo/guide/)
- [Rust新项目起步](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetimes.html)
- [一张快照：Rust项目的生命周期](https://smallcultfollowing.com/babysteps/blog/2018/08/20/a-tour-of-rust-in-ten-steps/)