---
title:    "Rust: 打印调试输出"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：

Rust 是一种功能强大的编程语言，它的特点包括高性能、安全性和并发性。打印调试输出是一种常见的调试手段，它可以帮助程序员在开发过程中更好地理解程序的运行情况。

## 如何打印调试输出

要在 Rust 中打印调试输出，需要使用 `println!` 宏。下面是一个简单的例子：

```Rust
let greetings = "你好！";
println!("这是一个打印调试输出的例子：{}", greetings);
```

运行结果将会是：

```
这是一个打印调试输出的例子：你好！
```
可以使用 `{}` 占位符来打印变量或表达式的值，也可以使用其他占位符来指定打印结果的格式。更多详细的用法可以查看官方文档。

## 深入了解打印调试输出

在 Rust 中，打印调试输出并不仅限于使用 `println!` 宏。还可以使用 `eprintln!` 宏将输出打印到标准错误流中，或者使用 `dbg!` 宏来将变量的值和相应的代码位置打印出来以辅助调试。

另外，Rust 还支持自定义打印格式，可以通过实现 `std::fmt::Display` 或 `std::fmt::Debug` trait 来控制打印的格式。这是一种更加灵活的调试方式，可以根据具体的情况来定制输出结果。

## 参考链接

- [Rust 官方文档：std::fmt](https://doc.rust-lang.org/std/fmt/index.html)
- [Rust 官方文档：std::debug](https://doc.rust-lang.org/std/fmt/debug/index.html)
- [Rust 官方文档：std::println](https://doc.rust-lang.org/std/macro.println.html)

## 参见

- [Rust 中最常用的调试方法](https://www.ruanyifeng.com/blog/2019/12/rust-debugging.html)
- [Rust 调试技巧方法总结](https://cloud.tencent.com/developer/article/1463625)
- [Rust 学习笔记：调试](http://www.sheshbabu.com/posts/rust-tricks-for-easy-debugging/)