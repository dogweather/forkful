---
title:                "使用TOML"
date:                  2024-01-26T04:26:18.940320-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
TOML 是一种人类可读的数据序列化语言，经常用于配置文件。程序员之所以使用 TOML，是因为它的简单和清晰，能够轻松转换成 Rust 中的哈希映射。

## 如何操作：
```Rust
// 1. 在你的 Cargo.toml 中包含 'toml' 包
// [dependencies]
// toml = "0.5"

// 2. 将 TOML 反序列化成 Rust 中的结构体
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("服务器运行在 {}:{}", host, port);
    // 输出：服务器运行在 "localhost":8080
}
```

## 深入探索
TOML，代表 Tom's Obvious, Minimal Language（汤姆的明显的、最小的语言），由 Tom Preston-Werner 在 2013 年创造。它旨在比 JSON 或 YAML 更可读，用于配置文件。TOML 的设计重点在于语法的无歧义性、最小主义以及轻松映射到数据类型上。

TOML 的替代品包括 JSON、YAML 和 XML，但在需要非程序员进行文件编辑和人类可读性至关重要的场景下，TOML 胜出。在 Rust 中使用 TOML 时，serde 提供了序列化和反序列化的坚实基础，通过 traits 无缝地将 TOML 映射到 Rust 的结构体上。

在使用 TOML 时面临的一个挑战是其对类型和结构的严格性。程序员必须定义一个反映 TOML 数据模式的良好结构化的 Rust 类型系统，才能有效利用 Rust 中的 TOML。

## 另请参阅
- [TOML 文档](https://toml.io/en/)
- [serde_toml 包](https://docs.rs/serde_toml/)
- [Rust 编程语言书籍](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub 仓库](https://github.com/toml-lang/toml)