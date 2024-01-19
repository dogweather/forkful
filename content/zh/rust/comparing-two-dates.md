---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
比较两个日期就是检查两个日期哪个更早，哪个更晚。程序员会做这个操作来对时间序列数据进行排序，或者在需要时间敏感性的计算中，

## 如何操作：
Rust 为此程序设计了一种非常简单，高效，并且安全的做法。我们使用标准库中的 `SystemTime` 结构

```Rust
use std::time::SystemTime;
fn main() {
    let now = SystemTime::now();
    let past = SystemTime::UNIX_EPOCH;
    println!("{}", now > past);
}
```
输出
```shell
true
```
上面的程序比较了现在的时间和 Unix 纪年。上述代码的输出是 `true`，因为现在的日期比 Unix 纪年要晚。

## 深入探讨：
在 20 世纪 70 年代， Unix 操作系统首次将时间戳引入计算机科学。日期比较根据这个概念施行。这个历史概念被现代化的 Rust 语言所采纳和扩展。

Rust 使用一个极其简洁的 `SystemTime`库来进行日期的对比。另一个可行的替代方案是 chrono 库，它提供了更多详细和复杂的日期和时间处理选项。然而，对于简单的日期比较， `SystemTime` 仍然是一个很好的选择。

在 `SystemTime` 结构中，日期被存储为自 Unix 纪年以来的纳秒数。在比较两个日期时，这些数量被直接比较，这使日期比较非常高效。

## 另请参见：
 - [SystemTime 详细说明](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
 - [chrono 库详细说明](https://docs.rs/chrono/0.4.19/chrono/)