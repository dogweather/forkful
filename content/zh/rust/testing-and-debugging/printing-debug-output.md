---
date: 2024-01-20 17:53:25.236126-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u663E\u793A\u7A0B\u5E8F\u8FD0\
  \u884C\u65F6\u5185\u90E8\u53D8\u91CF\u548C\u72B6\u6001\u7684\u8FC7\u7A0B\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u6765\u68C0\u67E5\u4EE3\u7801\u662F\u5426\u6309\u9884\
  \u671F\u5DE5\u4F5C\uFF0C\u5FEB\u901F\u5B9A\u4F4D\u95EE\u9898\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.086012-07:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u663E\u793A\u7A0B\u5E8F\u8FD0\
  \u884C\u65F6\u5185\u90E8\u53D8\u91CF\u548C\u72B6\u6001\u7684\u8FC7\u7A0B\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u6765\u68C0\u67E5\u4EE3\u7801\u662F\u5426\u6309\u9884\
  \u671F\u5DE5\u4F5C\uFF0C\u5FEB\u901F\u5B9A\u4F4D\u95EE\u9898\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
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
