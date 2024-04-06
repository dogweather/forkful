---
date: 2024-01-26 04:18:20.273697-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u622A\u81F3\u76EE\u524D\uFF0CRust \u8FD8\
  \u6CA1\u6709\u5B98\u65B9\u968F\u9644\u7684 REPL\u3002\u4F60\u53EF\u4EE5\u4F7F\u7528\
  \u7B2C\u4E09\u65B9\u5DE5\u5177\u5982 `evcxr_repl`\u3002\u901A\u8FC7 Cargo \u5B89\
  \u88C5\u5B83\uFF1A."
lastmod: '2024-04-05T22:38:46.683821-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u622A\u81F3\u76EE\u524D\uFF0CRust \u8FD8\
  \u6CA1\u6709\u5B98\u65B9\u968F\u9644\u7684 REPL\u3002\u4F60\u53EF\u4EE5\u4F7F\u7528\
  \u7B2C\u4E09\u65B9\u5DE5\u5177\u5982 `evcxr_repl`\u3002\u901A\u8FC7 Cargo \u5B89\
  \u88C5\u5B83\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
