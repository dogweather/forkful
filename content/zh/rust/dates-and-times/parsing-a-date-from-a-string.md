---
title:                "从字符串解析日期"
date:                  2024-02-03T19:15:21.986781-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
