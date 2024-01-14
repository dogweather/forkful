---
title:                "Rust: 获取当前日期"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么
在编程中，获取当前日期是一个非常常见的需求。它可以帮助我们记录事件、定时任务和监控数据，让我们的程序更加准确和有用。

# 如何进行
在Rust编程语言中，获取当前日期是非常简单的。我们可以使用标准库中的`chrono`模块来实现。下面是一个示例代码：

```Rust
use chrono::{Local, DateTime, Datelike};

fn main() {
    // 获取当前日期和时间
    let now: DateTime<Local> = Local::now();

    // 获取当前年份
    let year: i32 = now.year();

    // 获取当前月份
    let month: u32 = now.month();

    // 获取当前日期
    let day: u32 = now.day();

    // 打印输出
    println!("今天是{}年{}月{}日。", year, month, day);
}
```

以上代码输出结果为：

```
今天是2021年9月1日。
```

# 深入了解
在Rust中，`chrono`模块提供了许多有用的函数来操作日期和时间。例如，我们可以使用`format()`函数来自定义日期和时间的格式，还可以通过调用`naive_local()`函数来创建不带时区信息的日期和时间。此外，`chrono`还提供了处理时区、计算日期差等功能，我们可以根据实际需求来选择使用。

# 参考资料
- [Rust标准库文档 - chrono](https://doc.rust-lang.org/std/time/index.html)
- [Rust编程语言教程 - 时间和日期](https://rustcc.cn/article?id=9bdec26d-5409-40de-adf1-c45a4769d75e)
- [Rust Cookbook - 日期和时间](https://rust-lang-nursery.github.io/rust-cookbook/datetime)
- [Chrono官方文档](https://docs.rs/chrono/)