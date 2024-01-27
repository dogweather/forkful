---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
向标准错误写入信息（简称 stderr）用于报告程序错误。程序员这样做是为了将错误信息与正常输出分开，便于调试和日志记录。

## How to: 如何操作
Rust 中写入 stderr 通常使用 `eprintln!` 宏或 `std::io::stderr`。

```Rust
fn main() {
    // 使用 eprintln! 宏简单打印错误信息
    eprintln!("发生错误！");

    // 更复杂的写法
    use std::io::{self, Write};
    let stderr = &mut io::stderr();
    let _ = writeln!(stderr, "发生了另一个错误！");
}
```

输出样例：
```
发生错误！
发生了另一个错误！
```

## Deep Dive 深入探索
标准错误（stderr）是UNIX哲学的产物，目的是让正常输出和错误信息隔离。其他写入 stderr 的方法包括使用日志框架或作为 `std::io::Write` 特质的实现。在不同的操作系统和具体应用中，标准错误的处理可能有差异。

## See Also 更多资源
- Rust 官方文档关于 [`std::io`](https://doc.rust-lang.org/std/io/) 的说明
- [`eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html) 宏的详细资料
- [`Write`](https://doc.rust-lang.org/std/io/trait.Write.html) trait reference
