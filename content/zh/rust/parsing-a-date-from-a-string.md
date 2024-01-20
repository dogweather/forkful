---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
解析日期从字符串是一项将字符串数据转化为日期格式的过程。程序员这样做是因为这使得他们可以对日期进行操作，比如比较或排序。

## 如何操作：
下面的Rust代码示例演示了如何从字符串解析日期。

```Rust
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};
use std::str::FromStr;

fn main() {
    let date_string = "2022-03-04";
    let parsed_date = NaiveDate::parse_from_str(date_string, "%Y-%m-%d").unwrap();

    println!("{:?}", parsed_date);
}
```

运行上述代码，将会得到以下输出：

```Rust
NaiveDate { y: 2022, m: 3, d: 4 }
```

## 深入探讨
### 历史背景
UNIX系统在最早时期即开始使用字符串解析日期。这是因为初期的计算机硬件和软件对内存的需求迫切，且字符串是一种简洁、高效的存储方式。

### 替代方式
如果你不使用标准库，你还可以使用第三方库，比如`dateparser`，这是一个Python库，它可以解析任何包含日期和时间的字符串。

### 实施细节
Rust中的字符串解析日期是依赖于`chrono`库的。`NaiveDate::parse_from_str`方法使用格式字符串（例如`"%Y-%m-%d"`）解析日期，然后返回解析的日期。

## 查阅更多：
查阅更多关于在Rust中解析日期的信息，你可以访问以下网站：
- [`chrono` Crate Documentation | Docs.rs]: (https://docs.rs/chrono/)
- [The Rust Programming Language - Official Documentation]: (https://doc.rust-lang.org/book/)
- [StackOverflow - Parsing a date]: (https://stackoverflow.com/questions/41632114/how-to-parse-a-date-string-in-rust)