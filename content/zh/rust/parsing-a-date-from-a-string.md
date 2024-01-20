---
title:                "从字符串解析日期"
date:                  2024-01-20T15:38:18.558754-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
解析日期是指从字符串中提取出日期信息的过程。程序员这么做是因为我们需要将日期数据转换成计算机可以理解和操作的格式。

## 如何做：
```Rust
use chrono::{DateTime, NaiveDateTime, Utc, TimeZone};

fn main() {
    let date_string = "2023-04-01T12:34:56Z";
    let date = DateTime::parse_from_rfc3339(date_string).unwrap_or_else(|_| Utc.timestamp(0, 0));

    println!("{}", date);
}
```
输出：
```text
2023-04-01 12:34:56 UTC
```

## 深入了解
在 Rust 中，日期解析通常使用 `chrono` 这个库。`chrono` 是基于之前受欢迎的 `time` 库提供改进的。除 `chrono` 外，你还可以使用 `time` 或者 `date` 宏库，但 `chrono` 提供了更全面的功能。

将字符串解析为日期有不同的方法和格式。`rfc3339`和`iso8601`是最常用的国际标准。`chrono` 库支持从字符串解析多种不同的日期格式，并允许自定义格式。

这个过程中，错误处理非常重要。在 Rust 中，我们通常使用 `unwrap()`, `expect()` 或者匹配 `Result`类型 提供的方法来处理可能的错误，确保我们的程序在面对无效输入时不会崩溃。

## 参见
- [`chrono` 库文档](https://docs.rs/chrono/)
- [`time` 库文档](https://docs.rs/time/)
- [RFC 3339 Standard](https://www.ietf.org/rfc/rfc3339.txt)