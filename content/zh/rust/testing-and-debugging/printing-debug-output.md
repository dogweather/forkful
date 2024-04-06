---
date: 2024-01-20 17:53:25.236126-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) \u5728 Rust \u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `println!` \u5B8F\u6765\u6253\u5370\u8F93\u51FA\u3002\u5982\u679C\
  \u60F3\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\uFF0C\u4F7F\u7528 `println!(\"{:?}\"\
  , variable);`\u3002\u7528 `{:?}` \u8868\u793A\u7B26\u9700\u8981\u7C7B\u578B\u5B9E\
  \u73B0\u4E86 `Debug` trait\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.684856-06:00'
model: gpt-4-1106-preview
summary: "?}\", variable);`\u3002\u7528 `{:?}` \u8868\u793A\u7B26\u9700\u8981\u7C7B\
  \u578B\u5B9E\u73B0\u4E86 `Debug` trait\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## How to (如何操作)
在 Rust 中，你可以使用 `println!` 宏来打印输出。如果想打印调试信息，使用 `println!("{:?}", variable);`。用 `{:?}` 表示符需要类型实现了 `Debug` trait。

```Rust
fn main() {
    let my_var = 10;
    println!("普通输出: {}", my_var);  // 普通输出
    println!("调试输出: {:?}", my_var); // 调试输出
}
```
输出:

```
普通输出: 10
调试输出: 10
```

## Deep Dive (深入探索)
打印调试输出在 Rust 编程中至关重要。`println!` 宏的历史可以追溯到 Rust 语言的早期版本。Rust 也支持其他输出宏，比如 `debug!`，它属于 `log` crate，用于更复杂的日志记录。调试输出的实现需要类型实现 `std::fmt::Debug` trait，这通常通过在类型定义上添加 `#[derive(Debug)]` 属性来自动实现。

## See Also (另请参阅)
- Rust 文档中的 `println!` 宏: https://doc.rust-lang.org/std/macro.println.html
- `Debug` trait: https://doc.rust-lang.org/std/fmt/trait.Debug.html
- `log` crate 文档: https://docs.rs/log/
