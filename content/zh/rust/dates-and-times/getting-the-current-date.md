---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:51.576603-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust\u7684\u6807\u51C6\u5E93\u63D0\u4F9B\
  \u4E86\u4E00\u79CD\u6709\u9650\u4F46\u5FEB\u901F\u7684\u65B9\u5F0F\u6765\u83B7\u53D6\
  \u5F53\u524D\u65F6\u95F4\uFF0C\u867D\u7136\u4E0D\u662F\u4EE5\u65E5\u5386\u683C\u5F0F\
  \u76F4\u63A5\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u3002\u8FD9\u662F\u4F60\u5982\u4F55\
  \u505A\u5230\u7684\uFF1A."
lastmod: '2024-04-05T22:38:46.693277-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust\u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86\
  \u4E00\u79CD\u6709\u9650\u4F46\u5FEB\u901F\u7684\u65B9\u5F0F\u6765\u83B7\u53D6\u5F53\
  \u524D\u65F6\u95F4\uFF0C\u867D\u7136\u4E0D\u662F\u4EE5\u65E5\u5386\u683C\u5F0F\u76F4\
  \u63A5\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u3002\u8FD9\u662F\u4F60\u5982\u4F55\u505A\
  \u5230\u7684\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
