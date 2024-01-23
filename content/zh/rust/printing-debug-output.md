---
title:                "打印调试输出"
date:                  2024-01-20T17:53:25.236126-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

打印调试输出是显示程序运行时内部变量和状态的过程。程序员这样做来检查代码是否按预期工作，快速定位问题。

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
