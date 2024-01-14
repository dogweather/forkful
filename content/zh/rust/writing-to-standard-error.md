---
title:                "Rust: 写入标准错误"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：为什么要将内容写入标准错误？这是因为在程序运行时出现错误时，标准错误通常会被输出到日志文件中，供调试和错误追踪。因此，向标准错误写入内容可以帮助开发人员更轻松地分析和解决程序中的问题。

## 如何：

```rust
fn main() {
    eprintln!("这是示例文本，将被写入标准错误。");
}
```

当你运行这段代码时，文本“这是示例文本，将被写入标准错误。”会被写入标准错误并显示在终端窗口中。这样，你就可以在程序发生错误时快速定位问题所在，提高调试效率。

## 深入探讨：

除了简单的向标准错误打印文本，你也可以使用其它数据类型。比如：

```rust
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    for num in numbers.iter() {
        eprintln!("这是第{}个数字。", num);
    }
}
```

这样，你就可以在程序的运行过程中观察变量的值，有助于发现程序中潜在的错误。

## 参考链接：

[标准错误](https://doc.rust-lang.org/std/io/stderr/index.html)

[Rust语言简介](https://www.rust-lang.org/zh-CN/what.html)

[Mandarin Rust社区](https://sinovoip.chat/c/rust/)