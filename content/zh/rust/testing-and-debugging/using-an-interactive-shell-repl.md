---
date: 2024-01-26 04:18:20.273697-07:00
description: "Rust \u4EA4\u4E92\u5F0F shell\uFF0C\u6216 REPL\uFF08\u8BFB\u53D6-\u6C42\
  \u503C-\u6253\u5370 \u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\u80FD\u5373\u65F6\u8FD0\
  \u884C Rust \u4EE3\u7801\uFF0C\u770B\u5230\u5373\u65F6\u7ED3\u679C\uFF0C\u975E\u5E38\
  \u9002\u5408\u5B9E\u9A8C\u6216\u5B66\u4E60\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\
  \u6765\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\uFF0C\u6216\u53EA\u662F\
  \u73A9\u5F04\u8BED\u8A00\u7279\u6027\uFF0C\u65E0\u9700\u7F16\u8BD1\u6574\u4E2A\u9879\
  \u76EE\u7684\u5F00\u9500\u3002"
lastmod: '2024-02-25T18:49:45.084984-07:00'
model: gpt-4-0125-preview
summary: "Rust \u4EA4\u4E92\u5F0F shell\uFF0C\u6216 REPL\uFF08\u8BFB\u53D6-\u6C42\u503C\
  -\u6253\u5370 \u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\u80FD\u5373\u65F6\u8FD0\u884C\
  \ Rust \u4EE3\u7801\uFF0C\u770B\u5230\u5373\u65F6\u7ED3\u679C\uFF0C\u975E\u5E38\u9002\
  \u5408\u5B9E\u9A8C\u6216\u5B66\u4E60\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\
  \u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\uFF0C\u6216\u53EA\u662F\u73A9\
  \u5F04\u8BED\u8A00\u7279\u6027\uFF0C\u65E0\u9700\u7F16\u8BD1\u6574\u4E2A\u9879\u76EE\
  \u7684\u5F00\u9500\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
