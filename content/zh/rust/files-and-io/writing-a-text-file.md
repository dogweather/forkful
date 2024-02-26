---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:18.023531-07:00
description: "\u5728 Rust \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5728\
  \u6587\u4EF6\u7CFB\u7EDF\u4E0A\u521B\u5EFA\u3001\u5199\u5165\uFF0C\u4EE5\u53CA\u53EF\
  \u80FD\u5411\u6587\u4EF6\u8FFD\u52A0\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u6267\u884C\
  \u8FD9\u4E00\u64CD\u4F5C\u662F\u4E3A\u4E86\u6301\u4E45\u5316\u6570\u636E\uFF0C\u5982\
  \u5E94\u7528\u7A0B\u5E8F\u65E5\u5FD7\u3001\u914D\u7F6E\u6216\u7528\u6237\u751F\u6210\
  \u7684\u5185\u5BB9\uFF0C\u786E\u4FDD\u6570\u636E\u5728\u7A0B\u5E8F\u6267\u884C\u8303\
  \u56F4\u4E4B\u5916\u7684\u6301\u4E45\u6027\u3002"
lastmod: '2024-02-25T18:49:45.103898-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Rust \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5728\
  \u6587\u4EF6\u7CFB\u7EDF\u4E0A\u521B\u5EFA\u3001\u5199\u5165\uFF0C\u4EE5\u53CA\u53EF\
  \u80FD\u5411\u6587\u4EF6\u8FFD\u52A0\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u6267\u884C\
  \u8FD9\u4E00\u64CD\u4F5C\u662F\u4E3A\u4E86\u6301\u4E45\u5316\u6570\u636E\uFF0C\u5982\
  \u5E94\u7528\u7A0B\u5E8F\u65E5\u5FD7\u3001\u914D\u7F6E\u6216\u7528\u6237\u751F\u6210\
  \u7684\u5185\u5BB9\uFF0C\u786E\u4FDD\u6570\u636E\u5728\u7A0B\u5E8F\u6267\u884C\u8303\
  \u56F4\u4E4B\u5916\u7684\u6301\u4E45\u6027\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Rust 中写入文本文件涉及在文件系统上创建、写入，以及可能向文件追加数据。程序员执行这一操作是为了持久化数据，如应用程序日志、配置或用户生成的内容，确保数据在程序执行范围之外的持久性。

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
