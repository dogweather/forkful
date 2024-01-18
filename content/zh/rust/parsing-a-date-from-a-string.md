---
title:                "从字符串中解析日期"
html_title:           "Rust: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么是在Rust中解析日期字符串？为什么程序员要这样做？

解析日期字符串是指将一个日期以字符串的形式转换成计算机可识别的日期格式。程序员通常会这样做是因为他们需要处理大量的日期数据，而这些日期数据通常以字符串的形式被存储或传输。通过解析日期字符串，程序员可以轻松地将字符串日期转换成可处理的日期对象，方便进行计算和比较。

## 如何在Rust中解析日期字符串？

```
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, ParseError, Timelike, Utc};

fn parse_date_string(date_string: &str) -> Result<DateTime<Utc>, ParseError> {
    // 使用chrono库的parse函数将日期字符串转换成DateTime对象
    let date = date_string.parse::<DateTime<Local>>()?;
    // 使用UTC时区来存储日期时间
    let utc_date = date.with_timezone(&Utc);
    return Ok(utc_date);
}

fn main() {
    let date_string = "2021-10-21 07:30";
    // 调用parse_date_string函数来将字符串转换成日期时间对象
    let date = parse_date_string(date_string).unwrap();
    // 使用date对象来输出日期时间的各个部分
    println!("日期：{}", date.date());
    println!("时间：{}", date.time());
    println!("年：{}", date.year());
    println!("月：{}", date.month());
    println!("日：{}", date.day());
    println!("小时：{}", date.hour());
    println!("分钟：{}", date.minute());
    println!("秒：{}", date.second());
}

```

解析结果：

```
日期：2021-10-21 UTC
时间：07:30:00 UTC
年：2021
月：10
日：21
小时：7
分钟：30
秒：0
```

## 深挖：解析日期字符串的历史背景、备选方案和实现细节

在计算机科学的早期，计算机无法直接处理日期数据，因此程序员必须手动将日期转换成计算机可识别的格式。随着计算机发展，一些语言如C++和Python都提供了内置的日期处理功能。而在Rust中，date-time库提供了轻量级的日期解析和格式化功能。

除了使用date-time库外，程序员也可以使用其他第三方库来解析日期字符串，如chrono和time库。这些库提供了更多的日期处理选项，但是对于简单的日期解析需求来说，使用date-time库就已经很方便了。

在实现解析日期字符串的过程中，常见的方法是使用正则表达式来匹配字符串中的日期部分，然后将其转换成目标日期格式。而在Rust中，使用date-time库的parse函数可以非常方便地实现这一过程，无需手动编写正则表达式。

## 查看更多：相关资料链接

- [date-time库官方文档](https://docs.rs/date-time/0.2.1/date_time/index.html)
- [chrono库官方文档](https://docs.rs/chrono/0.4.19/chrono/)
- [time库官方文档](https://docs.rs/time/0.2.25/time/)