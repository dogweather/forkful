---
title:                "获取当前日期"
date:                  2024-01-20T15:16:51.447180-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
什么和为什么？获取当前日期可以帮助你记录事件发生的时间，用于日志、用户界面或时间戳。编程时常有这种需求，就像记录一个交易的时间或显示今天的日历。

## How to:
怎么办：

要在Rust中获取当前日期，我们会使用`chrono`这个crate。这是一个处理日期和时间的库。

首先，把`chrono`加到你的`Cargo.toml`：

```toml
[dependencies]
chrono = "0.4.19"
```

然后，以下面的代码为例来获取并打印当前日期：

```Rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let today = Local::today();
    println!("今天的日期是: {}-{}-{}", today.year(), today.month(), today.day());
}
```

运行程序应该会看到类似的输出：

```
今天的日期是: 2023-04-01
```

## Deep Dive
深入探索：

`chrono`是Rust中最受欢迎的日期和时间库。它基于C++的`boost::date_time`库，但有更安全、简洁的Rust风格。

你还可以用标准库中的`std::time`，但它只提供了一些基本功能。`chrono`提供了全面的日期和时间处理，包括时区和格式化。

实现细节上，`chrono`处理日期和时间是基于一个叫做`NaiveDateTime`的概念，这是一个不考虑时区的时间，然后结合时区信息来提供本地化的日期和时间。

## See Also
相关链接：

- `chrono` crate官方文档：https://docs.rs/chrono/0.4.19/chrono/
- Rust编程语言官网：https://www.rust-lang.org/
- Rust `std::time`模块文档：https://doc.rust-lang.org/std/time/index.html
