---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:18:20.273697-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么与为何？
Rust 交互式 shell，或 REPL（读取-求值-打印 循环），让你能即时运行 Rust 代码，看到即时结果，非常适合实验或学习。程序员使用它来测试代码片段、调试，或只是玩弄语言特性，无需编译整个项目的开销。

## 如何操作：
截至目前，Rust 还没有官方随附的 REPL。你可以使用第三方工具如 `evcxr_repl`。通过 Cargo 安装它：

```sh
cargo install evcxr_repl
```

然后，运行 REPL：

```sh
evcxr
```

在里面，测试一些 Rust 代码：

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

输出应该是：

```
5 + 3 = 8
```

## 深入探索
Rust 的精神核心围绕着安全与性能，这通常与需要提前编译的语言关联在一起，与解释型、适合 REPL 的语言联系不大。从历史上看，像 Python 或 Ruby 这样的语言优先考虑拥有 REPL 以获得即时反馈，但并不是为了系统级任务而设计的。

尽管 Rust 缺乏官方的 REPL，但像 `evcxr_repl` 这样的几种替代方案已经出现。这些项目不仅仅是将 Rust 黑入 REPL；它们巧妙地将语言的编译与运行周期整合到交互式会话中。REPL 在幕后编译并运行二进制文件，捕获输出。这样，它既保留了 Rust 的性能优势，同时还提供了交互式体验。

Rust 社区正在就官方支持 REPL 进行讨论，每一次语言迭代，我们都看到更多工具的复杂性，最终可能导致一个原生解决方案的出现。

## 另请参阅
更多信息和其他工具：
- Evcxr REPL GitHub 仓库：[https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground，一种在线测试 Rust 代码的方式：[https://play.rust-lang.org/](https://play.rust-lang.org/)
- Rust 语言关于 REPL 功能的讨论：[https://internals.rust-lang.org/](https://internals.rust-lang.org/)