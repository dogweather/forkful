---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:21.986781-07:00
description: "\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u662F\u5904\u7406\
  \u7528\u6237\u8F93\u5165\u6216\u4ECE\u6587\u4EF6\u8BFB\u53D6\u6570\u636E\u65F6\u7684\
  \u5E38\u89C1\u4EFB\u52A1\uFF0C\u5B83\u6D89\u53CA\u5C06\u5B57\u7B26\u4E32\u6570\u636E\
  \u8F6C\u6362\u4E3A\u7F16\u7A0B\u8BED\u8A00\u8BC6\u522B\u7684\u65E5\u671F\u683C\u5F0F\
  \u3002\u5728 Rust \u4E2D\uFF0C\u8FD9\u5BF9\u4E8E\u65E5\u671F\u64CD\u4F5C\u975E\u5E38\
  \u91CD\u8981\uFF0C\u5982\u6BD4\u8F83\u3001\u7B97\u672F\u8FD0\u7B97\u6216\u683C\u5F0F\
  \u5316\uFF0C\u5E76\u4E14\u5B83\u589E\u5F3A\u4E86\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\
  \u6570\u636E\u9A8C\u8BC1\u4E0E\u5B8C\u6574\u6027\u3002"
lastmod: '2024-03-13T22:44:47.531682-06:00'
model: gpt-4-0125-preview
summary: "\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u662F\u5904\u7406\
  \u7528\u6237\u8F93\u5165\u6216\u4ECE\u6587\u4EF6\u8BFB\u53D6\u6570\u636E\u65F6\u7684\
  \u5E38\u89C1\u4EFB\u52A1\uFF0C\u5B83\u6D89\u53CA\u5C06\u5B57\u7B26\u4E32\u6570\u636E\
  \u8F6C\u6362\u4E3A\u7F16\u7A0B\u8BED\u8A00\u8BC6\u522B\u7684\u65E5\u671F\u683C\u5F0F\
  \u3002\u5728 Rust \u4E2D\uFF0C\u8FD9\u5BF9\u4E8E\u65E5\u671F\u64CD\u4F5C\u975E\u5E38\
  \u91CD\u8981\uFF0C\u5982\u6BD4\u8F83\u3001\u7B97\u672F\u8FD0\u7B97\u6216\u683C\u5F0F\
  \u5316\uFF0C\u5E76\u4E14\u5B83\u589E\u5F3A\u4E86\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\
  \u6570\u636E\u9A8C\u8BC1\u4E0E\u5B8C\u6574\u6027\u3002."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 什么 & 为什么？

解析字符串中的日期是处理用户输入或从文件读取数据时的常见任务，它涉及将字符串数据转换为编程语言识别的日期格式。在 Rust 中，这对于日期操作非常重要，如比较、算术运算或格式化，并且它增强了应用程序中的数据验证与完整性。

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
