---
title:                "写入标准错误"
aliases:
- /zh/rust/writing-to-standard-error/
date:                  2024-02-03T19:34:37.662143-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Rust 中写入标准错误 (stderr) 是指将错误信息和诊断信息定向到与标准输出 (stdout) 分开的控制台。程序员这样做是为了区分正常程序输出和错误信息，使得更容易适当处理错误或在执行期间将其重定向到日志或文件。

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
