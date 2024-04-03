---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:37.662143-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\
  \u63A5\u5199\u5165 stderr \u7684\u7B80\u5355\u65B9\u5F0F\uFF0C\u4F7F\u7528\u7684\
  \u662F `eprintln!` \u5B8F\uFF0C\u7C7B\u4F3C\u4E8E `println!` \u7528\u4E8E stdout\
  \ \u7684\u65B9\u5F0F\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.539511-06:00'
model: gpt-4-0125-preview
summary: "Rust \u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u63A5\u5199\u5165 stderr \u7684\
  \u7B80\u5355\u65B9\u5F0F\uFF0C\u4F7F\u7528\u7684\u662F `eprintln!` \u5B8F\uFF0C\u7C7B\
  \u4F3C\u4E8E `println!` \u7528\u4E8E stdout \u7684\u65B9\u5F0F\u3002\u8FD9\u91CC\
  \u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作：
Rust 提供了一种直接写入 stderr 的简单方式，使用的是 `eprintln!` 宏，类似于 `println!` 用于 stdout 的方式。这里是一个基本示例：

```rust
fn main() {
    eprintln!("这是一个错误信息！");
}
```

示例输出（到标准错误）：
```
这是一个错误信息！
```

如果你想要对错误信息有更多控制，如当你希望格式化文本或处理 I/O 结果时，使用 `std::io` 模块中的 `stderr` 函数。此方法提供了一个句柄到全局的 stderr 流，然后你可以使用 `Write` 特征中的 `write_all` 或 `writeln` 方法向其写入：

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "格式化错误信息：{}", 404).expect("写入 stderr 失败");
}
```

示例输出（到标准错误）：
```
格式化错误信息：404
```

如果你在依赖库进行日志记录或错误处理的环境或应用程序中工作，如 `log` 和 `env_logger` 等库很受欢迎。虽然它们更多用于日志记录目的，但它们是可配置的，并且可以将错误日志级别定向到 stderr。下面是使用 `log` 和 `env_logger` 的简单示例：

首先，将依赖项添加到你的 `Cargo.toml`：
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

然后，在你的应用程序中设置并使用日志记录：
```rust
fn main() {
    env_logger::init();
    log::error!("这是记录到 stderr 的错误信息");
}
```

运行这个程序（在设置了适当的环境变量之后，例如，`RUST_LOG=error`），将会将错误信息输出到 stderr，利用日志记录基础设施。

```plaintext
ERROR: 这是记录到 stderr 的错误信息
```
