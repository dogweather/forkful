---
title:    "Rust: 比较两个日期"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

Rust程序设计语言已经成为许多开发者的首选，因为它具有强大的性能和安全性。它的标准库提供了许多有用的函数和工具，使得日常编程变得更加简单。在这篇博客文章中，我们将讨论如何使用Rust来比较两个日期，让你的日期处理更加轻松。

## 为什么需要比较日期

在编程中，经常需要比较日期来确定哪个日期在前还是在后，或者计算两个日期之间的差距。例如，在开发一个日历应用程序时，我们需要检查用户输入的日期是否合法并且在正确的顺序。因此，比较日期是一个非常常见的任务，也是一个很好的练习机会，来学习如何使用Rust的日期比较功能。

## 如何比较日期

首先，我们需要导入标准库中的`chrono`包，其中包含有关日期处理的函数和结构体。然后，我们可以使用`NaiveDate`结构体来表示具体的日期，它包含年、月、日的信息。以下是一个简单的例子，展示如何比较两个日期：

```Rust
// 导入chrono包
use chrono::prelude::*;

fn main() {
    // 创建两个NaiveDate结构体分别表示2020年1月1日和2020年1月2日
    let date_1 = NaiveDate::from_ymd(2020, 1, 1);
    let date_2 = NaiveDate::from_ymd(2020, 1, 2);

    // 使用cmp函数比较两个日期
    match date_1.cmp(&date_2) {
        Ordering::Less => println!("{} is before {}", date_1, date_2),
        Ordering::Equal => println!("{} is equal to {}", date_1, date_2),
        Ordering::Greater => println!("{} is after {}", date_1, date_2),
    }
}

// 输出：2020-01-01 is before 2020-01-02
```

在以上示例中，我们使用了`cmp`函数来比较两个日期，并使用`match`表达式来判断结果，输出对应的文字。此外，我们还可以使用`min`和`max`函数来确定两个日期中的最小值和最大值。

## 深入了解日期比较

在Rust的日期比较中，有一个重要的概念是“偏序”。偏序是指如果我们有两个日期`a`和`b`，并且`a < b`则认为`a`在`b`之前。然而，如果我们有两个日期`a`和`b`，并且`a`早于`b`或者`a`晚于`b`，那么我们无法确定`a`和`b`谁在谁的前面。

此外，Rust还提供了`DateTime`结构体来表示具体的日期和时间，可以用来比较精确到秒的日期。使用方法与`NaiveDate`类似。如果想要了解更多日期比较的信息，建议阅读官方文档和查看源码。

## 参考链接

- [The Rust Programming Language](https://www.rust-lang.org/zh-CN/)
- [Chrono - Date and time library for Rust](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust中的日期和时间](https://zhuanlan.zhihu.com/p/98473810)

## 参考文献

- [Rust Book 中文版](https://rustcc.gitbooks.io/rustprimer/content/type/boolean.html)
- [Rust标准库官方文档](https://doc.rust-lang.org/std/)