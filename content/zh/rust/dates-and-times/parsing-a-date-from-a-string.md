---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:21.986781-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust \u6807\u51C6\u5E93\u4E0D\u76F4\u63A5\
  \u5305\u542B\u65E5\u671F\u89E3\u6790\uFF0C\u4F46\u5E7F\u6CDB\u4F7F\u7528\u7684 `chrono`\
  \ \u5305\u662F\u4E00\u4E2A\u5065\u58EE\u7684\u65E5\u671F\u548C\u65F6\u95F4\u5904\
  \u7406\u89E3\u51B3\u65B9\u6848\u3002\u9996\u5148\uFF0C\u5728\u4F60\u7684 `Cargo.toml`\
  \ \u4E2D\u6DFB\u52A0 `chrono`\uFF1A."
lastmod: '2024-04-05T21:53:47.851792-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：


### 使用 Rust 的标准库（`chrono` 包）
Rust 标准库不直接包含日期解析，但广泛使用的 `chrono` 包是一个健壮的日期和时间处理解决方案。首先，在你的 `Cargo.toml` 中添加 `chrono`： 

```toml
[dependencies]
chrono = "0.4"
```

然后，使用 `chrono` 将日期字符串解析为 `NaiveDate` 对象：

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("解析日期失败");

    println!("解析后的日期：{}", date);
}

// 示例输出：
// 解析后的日期：2023-04-01
```

### 使用 Rust 的高级日期时间处理（`time` 包）
对于更高级的日期时间处理，包括更符合人体工学的解析，请考虑使用 `time` 包。首先，将其包含在你的 `Cargo.toml` 中：

```toml
[dependencies]
time = "0.3"
```

然后，使用 `Date` 类型和 `PrimitiveDateTime` 解析日期字符串：

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("解析日期时间失败");

    println!("解析后的日期时间：{}", parsed_date);
}

// 示例输出：
// 解析后的日期时间：2023-04-01 12:34:56
```

这两个示例展示了 Rust 如何借助第三方包来促进日期字符串解析为可操作的日期对象，使其成为涉及时间数据的软件开发中的强大工具。
