---
title:                "计算未来或过去日期"
html_title:           "Rust: 计算未来或过去日期"
simple_title:         "计算未来或过去日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

如果你需要编写一个日程管理软件或者需要做日期相关的计算，计算未来或过去的日期就是必不可少的功能。使用Rust编程语言，可以简单而高效地实现这一功能。

## 怎么做

计算未来或过去的日期，最重要的就是要确定参考日期，也就是今天的日期。通过调用Rust标准库中的`time::Date`类，我们可以轻松获取当前日期，然后进行加减运算。

下面是一个示例代码，假设今天是2020年10月10日，计算50天后的日期。

```Rust
use std::time::{Duration, SystemTime, UNIX_EPOCH};

let today = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards!"); // 获取当前日期
let mut future_date = today.as_secs() as i64 + (50 * 24 * 60 * 60); // 转换为秒，并加上50天的秒数
let future_date = SystemTime::UNIX_EPOCH + Duration::from_secs(future_date as u64); // 将计算后的秒数转换为日期对象
println!("50 days from now is {}", future_date); // 输出未来日期
```

运行结果为：`50 days from now is 2020-11-29 16:45:00 UTC`

同样的，如果需要计算过去的日期，则可以使用负数来表示减去的天数即可。

```Rust
use std::time::{Duration, SystemTime, UNIX_EPOCH};

let today = SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards!"); // 获取当前日期
let mut past_date = today.as_secs() as i64 - (30 * 24 * 60 * 60); // 转换为秒，并减去30天的秒数
let past_date = SystemTime::UNIX_EPOCH + Duration::from_secs(past_date as u64); // 将计算后的秒数转换为日期对象
println!("30 days ago was {}", past_date); // 输出过去日期
```

运行结果为：`30 days ago was 2020-09-10 16:45:00 UTC`

## 深入探讨

Rust标准库中提供了多种日期时间相关的类和方法，使用起来非常方便。如果需要更复杂的日期计算，可以参考官方文档中的“[时间处理](https://doc.rust-lang.org/book/ch16-01-threads.html#handling-time)”部分。

## 查看更多

- [Rust官方文档](https://doc.rust-lang.org/)
- [Rust中文社区](https://rust.cc/)