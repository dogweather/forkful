---
title:                "Rust: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么获取当前日期

Rust是一种功能强大且现代的编程语言，它已经在软件开发领域受到了广泛的关注。其中一个Rust的重要应用就是处理日期和时间的操作，获取当前日期也是其中之一。在本篇博客文章中，我们将会探讨为什么要获取当前日期，以及如何用Rust来实现这一操作。

## 如何获取当前日期

要在Rust中获取当前日期，我们需要使用标准库中的`chrono`模块。首先，我们需要在代码中引入该模块：

```Rust
use chrono::{Local, Date};
```

接着，我们可以用`Local::today()`来获取当天的日期，并将其存储在一个变量中：

```Rust
let today: Date<Local> = Local::today();
```

我们也可以获取年、月、日等具体的日期信息：

```Rust
let year = today.year();
let month = today.month();
let day = today.day();
```

最后，我们可以将日期信息打印出来，看看结果是什么：

```Rust
println!("当前日期是：{}年{}月{}日", year, month, day);
```

输出结果如下：

```
当前日期是：2021年11月4日
```

## 深入了解

在Rust中，日期和时间都是通过`DateTime`结构来表示的，该结构由`Date`和`Time`组成。另外，Rust的`chrono`模块也提供了许多其他的日期和时间操作，如计算两个日期之间的差值、格式化日期和时间等等。

此外，Rust的日期和时间操作也支持跨时区的处理，在处理跨时区的应用程序时，可以使用`TimeZone`结构来指定具体的时区。

# 参考链接

- [Rust Reference - 时间与日期](https://doc.rust-lang.org/reference/time-and-date.html)
- [Rust标准库 - chrono模块](https://doc.rust-lang.org/std/chrono/index.html)
- [Rust Cookbook - 处理日期和时间](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)

# 参见

- [Rust中处理字符串的方法](https://github.com/Mandarin-Programming-Olympiad/Blogs/blob/main/blogs/Rust%20%E4%B8%AD%E5%A4%84%E7%90%86%E5%AD%97%E7%AC%A6%E4%B8%B2.md)
- [Rust中的条件表达式](https://github.com/Mandarin-Programming-Olympiad/Blogs/blob/main/blogs/Rust%20%E4%B8%AD%E7%9A%84%E6%9D%A1%E4%BB%B6%E8%A1%A8%E8%BE%BE%E5%BC%8F.md)