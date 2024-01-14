---
title:                "Rust: 未来或过去计算日期"
simple_title:         "未来或过去计算日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

在编程世界中，经常会遇到需要计算未来或过去日期的情况。例如，计算一个产品的到期日期或者一个事件发生的日期。使用Rust编程语言可以轻松实现这样的计算，具有高效、安全和可靠的特点。

## 如何操作

使用Rust编程语言计算未来或过去的日期非常简单。首先，我们需要导入"chrono"包。然后，我们可以使用 `Local` 方法来创建本地日期对象。接下来，我们可以用 `+` 或 `-` 符号来对日期进行加减操作，例如 `date + Duration::weeks(2)` 表示增加两周后的日期。最后，可以通过 `format` 方法将日期格式化为我们需要的样式。

```Rust
// 导入chrono包
use chrono::{Local, Duration};

fn main() {
  // 创建一个本地日期对象
  let date = Local::today();

  // 计算10天后的日期
  let future_date = date + Duration::days(10);

  // 格式化日期
  let formatted_date = future_date.format("%Y-%m-%d");

  println!("10天后的日期是：{}", formatted_date);
}
// 输出： 10天后的日期是：2021-11-04
```

对于过去的日期计算，你也可以使用同样的方法，只需要使用 `-` 符号来表示减去的天数即可。

```Rust
// 导入chrono包
use chrono::{Local, Duration};

fn main() {
  // 创建一个本地日期对象
  let date = Local::today();

  // 计算10天前的日期
  let past_date = date + Duration::days(-10);

  // 格式化日期
  let formatted_date = past_date.format("%Y-%m-%d");

  println!("10天前的日期是：{}", formatted_date);
}
// 输出： 10天前的日期是：2021-10-15
```

## 深入了解

使用Rust进行日期计算并不仅限于加减操作，还可以通过 `DateTime` 对象来实现更复杂的功能。例如，`DateTime` 对象可以表示具体的某一秒，而 `Date` 对象只能精确到天。此外，Rust还提供了 `custom_format` 方法，可以自定义日期的格式输出。

此外，Rust还有许多其他强大的时间和日期处理库，如 `chrono-tz` 或 `time`，可以帮助我们更方便地处理复杂的日期和时间操作。

## 查看更多

- [官方文档：Rust编程语言](https://www.rust-lang.org/zh-CN/)
- [Chrono包: Rust官方日期和时间处理库](https://docs.rs/chrono/)
- [Chrono-tz包: 支持不同时区的日期和时间处理库](https://docs.rs/chrono-tz/)
- [Time包: Rust的高性能时间处理库](https://docs.rs/time/)