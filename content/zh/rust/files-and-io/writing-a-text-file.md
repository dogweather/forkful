---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:18.023531-07:00
description: "\u5982\u4F55\u505A\uFF1A Rust \u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\
  \u5F3A\u5927\u7684\u6587\u4EF6\u64CD\u4F5C\u5DE5\u5177\uFF0C\u4E3B\u8981\u5C01\u88C5\
  \u5728 `std::fs` \u548C `std::io` \u6A21\u5757\u4E2D\u3002\u8FD9\u91CC\u662F\u521B\
  \u5EFA\u548C\u5199\u5165\u6587\u672C\u6587\u4EF6\u7684\u57FA\u672C\u793A\u4F8B\uFF1A\
  ."
lastmod: '2024-04-05T21:53:47.861441-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

## 如何做：
Rust 的标准库提供了强大的文件操作工具，主要封装在 `std::fs` 和 `std::io` 模块中。这里是创建和写入文本文件的基本示例：

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

运行这段代码后，你会发现一个名为 `hello.txt` 的文件，内容为 "Hello, world!"。

对于更复杂的场景，如向文件追加内容或高效处理大量数据，Rust 提供了额外的功能。这里是如何向现有文件追加文本的示例：

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Adding more text.")?;
    Ok(())
}
```

运行这段代码将在 `hello.txt` 的末尾添加 " Adding more text."。

在某些案例中，利用第三方库可以简化文件操作。例如，`serde` 库结合 `serde_json`，允许将数据结构序列化和反序列化成 JSON 格式，为写文件提供了一个高级方法：

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

运行上述代码后，`user.json` 将包含 `User` 结构的 JSON 表示。注意，使用 `serde` 和 `serde_json` 需要将这些库添加到你的 `Cargo.toml` 中。

在 Rust 中写文本文件，无论是通过标准库还是借助外部库，都是一种简单而强大的方式，用于在应用程序中管理数据持久性。
