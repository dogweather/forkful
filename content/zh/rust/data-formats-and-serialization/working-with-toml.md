---
date: 2024-01-26 04:26:18.940320-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A TOML\uFF0C\u4EE3\u8868 Tom's Obvious,\
  \ Minimal Language\uFF08\u6C64\u59C6\u7684\u660E\u663E\u7684\u3001\u6700\u5C0F\u7684\
  \u8BED\u8A00\uFF09\uFF0C\u7531 Tom Preston-Werner \u5728 2013 \u5E74\u521B\u9020\
  \u3002\u5B83\u65E8\u5728\u6BD4 JSON \u6216 YAML \u66F4\u53EF\u8BFB\uFF0C\u7528\u4E8E\
  \u914D\u7F6E\u6587\u4EF6\u3002TOML\u2026"
lastmod: '2024-04-05T22:38:46.707760-06:00'
model: gpt-4-0125-preview
summary: "TOML \u7684\u66FF\u4EE3\u54C1\u5305\u62EC JSON\u3001YAML \u548C XML\uFF0C\
  \u4F46\u5728\u9700\u8981\u975E\u7A0B\u5E8F\u5458\u8FDB\u884C\u6587\u4EF6\u7F16\u8F91\
  \u548C\u4EBA\u7C7B\u53EF\u8BFB\u6027\u81F3\u5173\u91CD\u8981\u7684\u573A\u666F\u4E0B\
  \uFF0CTOML \u80DC\u51FA\u3002\u5728 Rust \u4E2D\u4F7F\u7528 TOML \u65F6\uFF0Cserde\
  \ \u63D0\u4F9B\u4E86\u5E8F\u5217\u5316\u548C\u53CD\u5E8F\u5217\u5316\u7684\u575A\
  \u5B9E\u57FA\u7840\uFF0C\u901A\u8FC7 traits \u65E0\u7F1D\u5730\u5C06 TOML \u6620\
  \u5C04\u5230 Rust \u7684\u7ED3\u6784\u4F53\u4E0A\u3002"
title: "\u4F7F\u7528TOML"
weight: 39
---

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
