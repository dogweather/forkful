---
aliases:
- /zh/rust/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:37.662143-07:00
description: "\u5728 Rust \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\
  \u6307\u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5B9A\u5411\u5230\
  \u4E0E\u6807\u51C6\u8F93\u51FA (stdout) \u5206\u5F00\u7684\u63A7\u5236\u53F0\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\u6B63\u5E38\u7A0B\
  \u5E8F\u8F93\u51FA\u548C\u9519\u8BEF\u4FE1\u606F\uFF0C\u4F7F\u5F97\u66F4\u5BB9\u6613\
  \u9002\u5F53\u5904\u7406\u9519\u8BEF\u6216\u5728\u6267\u884C\u671F\u95F4\u5C06\u5176\
  \u91CD\u5B9A\u5411\u5230\u65E5\u5FD7\u6216\u6587\u4EF6\u3002"
lastmod: 2024-02-18 23:08:58.955638
model: gpt-4-0125-preview
summary: "\u5728 Rust \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF (stderr) \u662F\u6307\
  \u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5B9A\u5411\u5230\u4E0E\
  \u6807\u51C6\u8F93\u51FA (stdout) \u5206\u5F00\u7684\u63A7\u5236\u53F0\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\u6B63\u5E38\u7A0B\u5E8F\
  \u8F93\u51FA\u548C\u9519\u8BEF\u4FE1\u606F\uFF0C\u4F7F\u5F97\u66F4\u5BB9\u6613\u9002\
  \u5F53\u5904\u7406\u9519\u8BEF\u6216\u5728\u6267\u884C\u671F\u95F4\u5C06\u5176\u91CD\
  \u5B9A\u5411\u5230\u65E5\u5FD7\u6216\u6587\u4EF6\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
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
