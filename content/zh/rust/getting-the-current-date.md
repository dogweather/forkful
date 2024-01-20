---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么?
获取当前日期是一种编程任务，它允许您的程序知道现在的日期。程序员这样做是因为他们需要在他们的程序中以某种方式与时间交互，比如计算持续时间，或者在某个具体的日期和时间执行任务。

## 如何做:
```Rust
use chrono::{Datelocal, Local};

fn main() {
    let now = Local::now();
    println!("{}", now.format("%Y-%m-%d"));
}
```
运行以上代码将会打印出当前日期，格式是 "YYYY-MM-DD"。

再看看如何获取当前的日期和时间:
```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now);
}
```

## 深入挖掘
历史上，Rust语言在日期和时间处理方面并不像某些其他语言那么强大，也没有内置的日期和时间库。 chrono是Rust中最广泛使用的日期和时间库，它提供了一个全面，有效的日期和时间处理解冓方。

至于获取当前日期的替代方案，在某些情况下，您可能不需要足够准确的日期和时间。 在这种情况下，您可以使用标准库中的SystemTime:
```Rust
use std::time::SystemTime;

fn main() {
    let now = SystemTime::now();
    println!("{:?}", now);
}
```
但是，记住这个方法提供的是自UNIX epoch以来的毫秒数，而不是具体的日期格式。

## 参考资料
* chrono库的文档：[https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
* Rust官方时间库的文档：[https://doc.rust-lang.org/stable/std/time/](https://doc.rust-lang.org/stable/std/time/)