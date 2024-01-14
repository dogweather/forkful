---
title:    "Rust: 把日期转换成字符串"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 为什么

日期和字符串是编程中经常会遇到的数据类型。将日期转换成字符串可以让我们更容易地在代码中处理和展示日期信息。在这篇文章中，我将会介绍如何使用Rust语言将日期转换成字符串。

## 如何进行

在Rust中，我们可以使用标准库中的 `format!` 宏来将日期转换成字符串。我们只需传入日期和想要的格式，就可以得到一个字符串。下面是一个例子：

```Rust
use chrono::{Utc, NaiveDate}; 

let birthday = NaiveDate::from_ymd(1990, 1, 1); 
let formatted_date = format!("{:?}", birthday); 

println!("{}", formatted_date); 
```

输出将会是 `1990-01-01`。我们也可以指定自己想要的日期格式，比如将日期转换成 `YYYY/MM/DD` 的格式：

```Rust
use chrono::{Utc, NaiveDate, Datelike}; 

let today = Utc::today().naive_utc(); 
let formatted_date = format!("{}/{}/{}", today.year(), today.month(), today.day()); 

println!("{}", formatted_date); 
```

输出将会是 `2021/07/26`。

## 深入探讨

在上面的例子中，我们使用了 `format!` 宏来进行日期格式化，这个宏底层其实是使用了 `to_string` 方法来将日期类型转换成字符串类型。`to_string` 方法是 `ToString` trait 的一部分，这个 trait 定义了如何将一个类型转换成字符串。在 Rust 中，实现了 `ToString` trait 的类型可以通过 `to_string` 方法来将自身转换成字符串。

除了使用 `format!` 宏，我们也可以直接使用 `to_string` 方法来进行日期转换。但需要注意的是，`to_string` 方法返回的是一个 `Result<String, _>` 类型，所以我们也需要处理可能发生的错误。

## 参考链接

- [Rust中的日期和时间](https://rust-lang-nursery.github.io/rust-cookbook/datetime/datetime.html)
- [Rust标准库中的日期和时间](https://doc.rust-lang.org/std/time/)
- [Rust标准库中的`ToString` trait](https://doc.rust-lang.org/std/string/trait.ToString.html)

## 参见

- [Rust文档](https://www.rust-lang.org/zh-CN/)
- [Rust中文](https://rustlang-cn.org/)