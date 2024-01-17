---
title:                "获取当前日期"
html_title:           "Rust: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Rust当前版本：获取当前日期的编程指南

## 什么是获取当前日期？为什么程序员要做这个？

获取当前日期是指在程序中获取当前的日期信息，包括年、月、日、时、分、秒等。程序员通常需要这项能力来跟踪程序运行的时间，用于调试、记录日志或生成时间相关的事件。

## 如何做到？

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let current_time = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards").as_secs();
println!("Current time in seconds since UNIX epoch: {}", current_time);
```

```Rust
use chrono::prelude::*;

let current_time = Utc::now();
let (year, month, day) = (current_time.year(), current_time.month(), current_time.day());
println!("{}/{}/{}", year, month, day);
```

输出：

Current time in seconds since UNIX epoch: 1626281807

2021/7/14

## 深入了解

获取当前日期在编程中非常常见，并且在计算机领域有着悠久的历史。在Rust中，有多种方法可以获取当前日期，如使用系统时间来计算以秒为单位的时间戳，或使用第三方库提供的日期时间对象来精确地获取年、月、日等信息。

除了系统时间和第三方库，还有其他一些可选的方法来获取当前日期，比如调用外部命令或API来获取网络时间。不同的方法适用于不同的情况，程序员可以根据自己的需求选择合适的方法。

在Rust中，获取当前日期的实现主要依赖于计算机的操作系统和硬件。因此，不同的操作系统和硬件可能会有细微的差异，程序员需要注意这一点。

## 参考资料

- Rust官方文档：https://www.rust-lang.org/zh-CN/

- chrono第三方库文档：https://docs.rs/chrono/0.4.19/chrono/

- 时间戳的概念和使用方法：https://web.evget.com/article/2019/5/23/54987.html