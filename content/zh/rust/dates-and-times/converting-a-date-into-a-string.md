---
title:                "将日期转换为字符串"
aliases:
- /zh/rust/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:26.064471-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## 什么与为什么？

将日期转换为字符串是把日期格式转换为文本格式的过程。程序员这么做主要是为了显示、存储或者在不同系统间传输日期数据。

## How to:
## 如何操作：

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let date_string = now.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", date_string);
}
```
输出样例：
```
2023-04-06 14:20:35
```

## Deep Dive
## 深入探究：

Rust 采用了`chrono`这个库来处理日期和时间。`chrono`比起 Rust 标准库提供了更全面的日期时间功能。历史上，不同文化有不同计时方法，但今天大多遵循 ISO 8601 标准，`chrono` 默认支持这个。

替代品有 time 和 date包，但`chrono`提供的`.format()`是最灵活的。性能上，日期转字符串不是重计算操作，但格式处理要考虑效率。

## See Also
## 另请参阅：

- Chrono 文档：https://docs.rs/chrono/
- Rust 日期时间处理相关讨论：https://users.rust-lang.org/t/how-to-deal-with-datetimes/2884
- ISO 8601 标准细节：https://www.iso.org/iso-8601-date-and-time-format.html
