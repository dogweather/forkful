---
title:                "获取当前日期"
aliases:
- /zh/rust/getting-the-current-date.md
date:                  2024-02-03T19:10:51.576603-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在Rust中获取当前日期是一个常见的任务，用于日志记录、基于时间的操作，或仅仅是显示日期。与一些在其标准库中包含日期和时间功能的语言不同，Rust鼓励使用一个健壮的第三方库，chrono，来进行全面的日期和时间操作，因为它的功能更优越，使用起来也更加容易。

## 如何操作：

### 使用Rust的标准库
Rust的标准库提供了一种有限但快速的方式来获取当前时间，虽然不是以日历格式直接获取当前日期。这是你如何做到的：

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("当前时间: {} 秒自 Unix 纪元以来。", n.as_secs()),
        Err(_) => panic!("SystemTime 在 Unix 纪元之前！"),
    }
}
```

输出：
```
当前时间: 1615390665 秒自 Unix 纪元以来。
```

### 使用 Chrono 库
为了获取更全面的日期和时间功能，包括获取当前日期，你应该使用 `chrono` 库。首先，将 `chrono` 添加到你的 `Cargo.toml` 中：

```toml
[dependencies]
chrono = "0.4"
```

然后，你可以使用 `chrono` 来获取当前日期：

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("当前日期: {}-{}-{}", now.year(), now.month(), now.day());
}
```

输出：
```
当前日期: 2023-4-20
```

`chrono` 库使得与日期和时间的工作变得直接，提供了一系列功能，不仅仅是检索当前日期，包括解析、格式化和对日期和时间进行算术操作。
