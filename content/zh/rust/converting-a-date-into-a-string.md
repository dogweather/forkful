---
title:    "Rust: 将日期转换为字符串。"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
日期转化成字符串是编程中一个常用的操作，它可以让计算机理解和处理日期数据，使其更易于读取和保存。

## 怎么做
```Rust
use chrono::{DateTime, Local, Utc};

let now: DateTime<Utc> = Utc::now();
println!("当前时间: {}", now);

let date_str = now.to_string();
println!("转化后的时间字符串: {}", date_str);

```

上面的代码演示了如何使用 Rust 中的 `chrono` 库来将 `DateTime` 类型的日期转化为字符串，并将其输出到控制台。使用此方法，您可以轻松地将日期数据转换为其他格式，以适合不同的需求。

## 深入了解
日期转化为字符串的过程实际上涉及多个步骤。首先，计算机需要将日期数据解析为特定的格式，例如 ISO 8601 标准。然后，它会根据指定的格式将数据转换为字符串。最后，程序将使用所选的编码来编码字符串，使其可读性更强。

## 参考链接
- [Rust Documentation - chrono](https://docs.rs/chrono/latest/chrono/)
- [Rust Basic Date and Time](https://drgarcia1986.gitbooks.io/rust_code_camp/content/Chapter_4_Basic_Date_and_Time.html)
- [Rust Cookbook - Format DateTime as String](https://rust-lang-nursery.github.io/rust-cookbook/datetime/format.html)