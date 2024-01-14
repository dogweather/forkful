---
title:    "Rust: 比较两个日期"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么

日期比较是程序员们经常遇到的任务，无论是为了验证用户输入的日期是否正确，还是为了计算两个日期之间的时间间隔。Rust提供了强大的日期比较功能，可以让我们更方便地处理这些任务。

## 如何

比较两个日期的最简单方法是使用`PartialOrd` trait中的`partial_cmp`函数。下面的例子演示了如何使用这个函数来比较两个日期，并打印出比较结果。

```Rust
use std::cmp::Ordering;
use chrono::{NaiveDate, Duration};

let date1 = NaiveDate::from_ymd(2021, 10, 1);
let date2 = NaiveDate::from_ymd(2021, 11, 1);

match date1.partial_cmp(&date2) {
    Some(Ordering::Less) => println!("{} is before {}", date1, date2),
    Some(Ordering::Equal) => println!("{} is equal to {}", date1, date2),
    Some(Ordering::Greater) => println!("{} is after {}", date1, date2),
    None => println!("Invalid dates"),
}
```

输出结果：

> 2021-10-01 is before 2021-11-01

如果想要计算两个日期之间的天数差，可以使用`difference`函数。下面的例子演示了如何计算两个日期相差的天数，并打印出结果。

```Rust
use chrono::{NaiveDate, Duration};

let date1 = NaiveDate::from_ymd(2021, 10, 1);
let date2 = NaiveDate::from_ymd(2021, 11, 1);

let diff = date2.signed_duration_since(date1);

println!("The difference is {} days", diff.num_days());
```

输出结果：

> The difference is 31 days

## 深入探讨

在Rust中，日期比较是基于`PartialOrd` trait实现的。这个trait定义了一个`partial_cmp`函数，用于比较两个日期的大小关系。在日期比较中，Rust会先比较日期的年份，如果一样则比较月份，再一样则比较天数。如果有一个日期的年份、月份或天数小于另一个日期，则前者小于后者。

除了对日期进行比较外，Rust还提供了丰富的日期操作方法，如计算两个日期之间的时间间隔、在日期上增减特定的时间单位等等。

## 参考资料

- [Rust Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Standard Library Documentation - PartialOrd](https://doc.rust-lang.org/stable/std/cmp/trait.PartialOrd.html)