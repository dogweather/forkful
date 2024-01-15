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

## 为什么要获取当前日期

日期是我们日常生活中不可或缺的一部分，它帮助我们记录时间、安排日程和保持组织。在编程中，获取当前日期也是非常重要的，它可以用于日志记录、数据分析、定时任务等多种场景。Rust提供了简单易用的函数来获取当前日期，让我们一起来看看如何实现吧！

## 如何获取当前日期

首先，我们需要在代码中导入日期相关的模块，例如 `time` 模块和 `DateTime` 结构体。

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use chrono::{DateTime, Utc};
```

然后，我们可以使用 `now()` 方法来获取当前日期和时间，并将其存储在 `DateTime` 结构体中。

```Rust
let now = DateTime::from(SystemTime::now());
```

最后，我们可以使用 `to_string()` 方法将日期转换为字符串格式，并打印出来。

```Rust
println!("当前日期：{}", now.to_string());
```

输出结果可能类似于：`当前日期：2021-07-30 19:30:00 UTC`，这取决于你所在的时区和具体时间。

## 深入了解获取当前日期

在Rust中， `DateTime` 结构体是基于 `ISO 8601` 标准来表示日期和时间的。它提供了许多有用的方法来处理日期，例如获取特定的时间信息、转换为其他日期格式等。此外，Rust还提供了 `time` 模块来处理更细致的日期操作，例如计算时间间隔、比较日期等。

## 参考链接

- [Rust官方文档 - time模块](https://doc.rust-lang.org/std/time/index.html)
- [Chrono文档](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [ISO 8601标准](https://www.iso.org/iso-8601-date-and-time-format.html)

## 查看更多

- [Rust入门教程](https://rustlang-cn.org/learn/get-started/) 
- [Rust编程语言官网](https://www.rust-lang.org/zh-CN)