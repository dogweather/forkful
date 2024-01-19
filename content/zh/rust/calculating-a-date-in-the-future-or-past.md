---
title:                "计算未来或过去的日期"
html_title:           "Rust: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

计算未来或过去的日期是指确定某个特定日期之前或之后的具体日期。程序员之所以会做这个，是因为它在处理时间线、设置提醒、或者生成报告等场景中非常有用。

## 操作方法：

下面是使用Rust编程语言为日期添加以及减少天数的简单例子。

```Rust
use chrono::{Date, Utc, Duration};

fn main() {
    let today: Date<Utc> = Utc::today(); 
    let future_date: Date<Utc> = today + Duration::days(5); 
    println!("{:?}", future_date); 

    let past_date: Date<Utc> = today - Duration::days(5);
    println!("{:?}", past_date); 
}
```
当你运行上述代码时，你会得到今天之后5天的日期以及今天之前5天的日期。

## 深入探讨：

计算未来或过去的日期的需求可以追溯到早期的计算机时代，当时人们需要预测和记录经济、科学、天文等方面的事件。对于Rust语言而言，“chrono”库是处理日期和时间的首选，其主要优势在于其丰富的功能和出色的性能。

此外，还有一些其他的库可以用来处理日期和时间的计算，例如"time"库。在某些情况下，选择哪个库要根据特定的需求和性能要求来决定。

"chrono"库中的Duration类用来表示时间间隔。在这个类中，“+”和“-”操作符被重载，用来做日期的加减。

## 另请参阅：

以下链接提供了更多关于Rust和日期处理的信息：

1. 官方Rust文档: [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
2. Chrono库文档: [https://docs.rs/chrono/0.4.13/chrono/](https://docs.rs/chrono/0.4.13/chrono/)
3. Time库文档： [https://docs.rs/time/0.1.43/time/](https://docs.rs/time/0.1.43/time/)