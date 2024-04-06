---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:35.369858-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust \u7684\u6807\u51C6\u5E93 (`std`)\
  \ \u5305\u62EC\u4E86\u901A\u8FC7 `std::path::Path` \u548C `std::fs` \u6A21\u5757\
  \u6765\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u529F\u80FD\u3002\u8FD9\
  \u662F\u4E00\u4E2A\u4F7F\u7528 Rust \u6807\u51C6\u65B9\u6CD5\u7684\u7B80\u5355\u793A\
  \u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:46.697525-06:00'
model: gpt-4-0125-preview
summary: ":path::Path` \u548C `std::fs` \u6A21\u5757\u6765\u68C0\u67E5\u76EE\u5F55\
  \u662F\u5426\u5B58\u5728\u7684\u529F\u80FD\u3002\u8FD9\u662F\u4E00\u4E2A\u4F7F\u7528\
  \ Rust \u6807\u51C6\u65B9\u6CD5\u7684\u7B80\u5355\u793A\u4F8B\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作：
Rust 的标准库 (`std`) 包括了通过 `std::path::Path` 和 `std::fs` 模块来检查目录是否存在的功能。这是一个使用 Rust 标准方法的简单示例：

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("目录存在。");
    } else {
        println!("目录不存在。");
    }
}
```

假设目录存在的情况下的示例输出：
```
目录存在。
```

对于更复杂的场景或增强的功能（如异步文件系统操作），你可能会考虑使用第三方库，如 `tokio` 及其异步 `fs` 模块，特别是如果你在异步运行时中工作。以下是如何使用 `tokio` 实现相同功能：

首先，在你的 `Cargo.toml` 中添加 `tokio`：

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

然后，使用 `tokio::fs` 异步检查目录是否存在：

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("目录存在。");
            } else {
                println!("路径存在但不是目录。");
            }
        },
        Err(_) => println!("目录不存在。"),
    }
}
```

假设目录不存在的情况下的示例输出：
```
目录不存在。
```

这些示例突出了 Rust 及其生态系统如何提供同步和异步的目录存在性检查方法，以满足广泛的软件开发需求。
