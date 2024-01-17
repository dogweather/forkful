---
title:                "将日期转换为字符串"
html_title:           "Rust: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 什么是日期转换成字符串?
日期转换字符串是指将日期对象转换为可读的字符串格式。程序员通常会这样做是因为他们需要向用户显示日期或将日期存储为字符串格式。

## 如何实现:
有几种不同的方法可以将日期转换为字符串。下面是使用Rust语言的两个例子，分别使用标准库和第三方库。

```
use std::time::SystemTime;

let now = SystemTime::now();
let string = format!("{}", now); // 将当前时间转换为字符串
println!("{}", string);
```

```
use chrono::{Utc, DateTime, Timelike};

let now: DateTime<UTC> = Utc::now();
let string = now.format("%Y%m%d").to_string(); // 将当前日期转换为字符串，格式为年月日
println!("{}", string);
```

输出:
```
2021-09-21 22:15:25.385983 UTC
20210921
```

## 深入了解:
日期转换字符串有一个长期的历史。在早期的计算机系统中，并没有内置的日期转换功能，程序员必须自己实现。现在，几乎所有的编程语言都提供了日期转换的内置函数或库，使得程序员的工作更加简单。除了使用标准库和第三方库，还可以使用字符串处理函数将日期转换为指定格式的字符串。

## 相关链接:
- [Rust标准库文档](https://doc.rust-lang.org/std/) 
- [Chrono库文档](https://crates.io/crates/chrono)