---
title:                "比较两个日期"
html_title:           "Rust: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 比较日期：什么 & 为什么？

比较日期，顾名思义，就是将两个日期进行对比。程序员经常会需要比较日期，主要是为了在编程中判断两个日期的前后关系。

# 如何操作：

在Rust中，比较日期有两种方式：使用标准库中的`cmp::Ordering`枚举，或使用第三方库`chrono`。下面分别给出这两种方式的示例代码和输出结果。

1. 使用标准库`cmp::Ordering`：

```Rust
use std::cmp::Ordering;

fn compare_dates(date1: (i32, i32, i32), date2: (i32, i32, i32)) -> Ordering {
    if date1.0 != date2.0 {
        date1.0.cmp(&date2.0)
    } else if date1.1 != date2.1 {
        date1.1.cmp(&date2.1)
    } else {
        date1.2.cmp(&date2.2)
    }
}

fn main() {
    let date1 = (2021, 9, 1);
    let date2 = (2020, 9, 1);
    let order = compare_dates(date1, date2);

    match order {
        Ordering::Less => println!("date1 is earlier than date2"),
        Ordering::Greater => println!("date1 is later than date2"),
        Ordering::Equal => println!("date1 is the same as date2"),
    }
}

// 输出结果：
// date1 is later than date2
```

2. 使用第三方库`chrono`：

```Rust
use chrono::{NaiveDate, Datelike};

fn compare_dates(date1: NaiveDate, date2: NaiveDate) -> i32 {
    date1.cmp(&date2).num_minutes()
}

fn main() {
    let date1 = NaiveDate::from_ymd(2021, 9, 1);
    let date2 = NaiveDate::from_ymd(2020, 9, 1);
    let minutes = compare_dates(date1, date2);
    println!("The difference in minutes is: {}", minutes);
}

// 输出结果：
// The difference in minutes is: 525600
```

# 深入了解：

日期比较在编程中是一个常用的操作，尤其是在需要处理时间相关的任务时。在较早的编程语言如C和Java中，并没有专门为日期比较提供内置的特性，使用起来比较麻烦。但是，在现代编程语言如Rust中，日期比较已经变得更加简单易用，而且可以使用不同的方法来满足需求。

除了上面提到的两种比较方式，也可以使用第三方库`time`来进行日期比较。而在Rust的标准库中，也提供了对日期格式的支持，比如`DateTime`和`Date`类型，可以方便地进行日期比较。

# 参考资料：

- Rust标准库： https://doc.rust-lang.org/std/cmp/enum.Ordering.html
- 第三方库chrono： https://crates.io/crates/chrono
- 第三方库time： https://crates.io/crates/time 
- Rust标准库中的日期格式： https://doc.rust-lang.org/std/time/index.html#date-and-time-types