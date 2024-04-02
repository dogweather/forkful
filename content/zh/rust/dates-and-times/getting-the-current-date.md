---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:51.576603-07:00
description: "\u5728Rust\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\
  \u5E38\u89C1\u7684\u4EFB\u52A1\uFF0C\u7528\u4E8E\u65E5\u5FD7\u8BB0\u5F55\u3001\u57FA\
  \u4E8E\u65F6\u95F4\u7684\u64CD\u4F5C\uFF0C\u6216\u4EC5\u4EC5\u662F\u663E\u793A\u65E5\
  \u671F\u3002\u4E0E\u4E00\u4E9B\u5728\u5176\u6807\u51C6\u5E93\u4E2D\u5305\u542B\u65E5\
  \u671F\u548C\u65F6\u95F4\u529F\u80FD\u7684\u8BED\u8A00\u4E0D\u540C\uFF0CRust\u9F13\
  \u52B1\u4F7F\u7528\u4E00\u4E2A\u5065\u58EE\u7684\u7B2C\u4E09\u65B9\u5E93\uFF0Cchrono\uFF0C\
  \u6765\u8FDB\u884C\u5168\u9762\u7684\u65E5\u671F\u548C\u65F6\u95F4\u64CD\u4F5C\uFF0C\
  \u56E0\u4E3A\u5B83\u7684\u529F\u80FD\u66F4\u4F18\u8D8A\uFF0C\u4F7F\u7528\u8D77\u6765\
  \u4E5F\u66F4\u52A0\u5BB9\u6613\u3002"
lastmod: '2024-03-13T22:44:47.533143-06:00'
model: gpt-4-0125-preview
summary: "\u5728Rust\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\u5E38\
  \u89C1\u7684\u4EFB\u52A1\uFF0C\u7528\u4E8E\u65E5\u5FD7\u8BB0\u5F55\u3001\u57FA\u4E8E\
  \u65F6\u95F4\u7684\u64CD\u4F5C\uFF0C\u6216\u4EC5\u4EC5\u662F\u663E\u793A\u65E5\u671F\
  \u3002\u4E0E\u4E00\u4E9B\u5728\u5176\u6807\u51C6\u5E93\u4E2D\u5305\u542B\u65E5\u671F\
  \u548C\u65F6\u95F4\u529F\u80FD\u7684\u8BED\u8A00\u4E0D\u540C\uFF0CRust\u9F13\u52B1\
  \u4F7F\u7528\u4E00\u4E2A\u5065\u58EE\u7684\u7B2C\u4E09\u65B9\u5E93\uFF0Cchrono\uFF0C\
  \u6765\u8FDB\u884C\u5168\u9762\u7684\u65E5\u671F\u548C\u65F6\u95F4\u64CD\u4F5C\uFF0C\
  \u56E0\u4E3A\u5B83\u7684\u529F\u80FD\u66F4\u4F18\u8D8A\uFF0C\u4F7F\u7528\u8D77\u6765\
  \u4E5F\u66F4\u52A0\u5BB9\u6613\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
