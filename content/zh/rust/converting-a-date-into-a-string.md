---
title:                "Rust: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编写任何程序时，我们经常需要将日期转换为字符串。这可以用于打印到屏幕上，也可以用于存储在数据库中。使用Rust语言，可以轻松地将日期转换为字符串。

# 怎么做

首先，我们需要导入`chrono`库来处理日期。然后，我们可以使用`Utc::today()`来获取当前日期，并使用`format()`函数将其转换为所需的格式。

```Rust
use chrono::{Utc, DateTime};

let date = Utc::today();

let date_string = date.format("%Y-%m-%d").to_string();

println!("Today's date is {}", date_string);
```

输出将是`Today's date is 2021-11-05`。

如果我们想要将具有时区信息的日期转换为字符串，我们可以使用`DateTime`类型，并使用`with_timezone()`函数指定所需的时区。

```Rust
use chrono::{Utc, DateTime, FixedOffset};

let date = Utc::today();

let date_timezone = date.with_timezone(&FixedOffset::east(8)); //将日期转换为东八区时间

let date_string = date_timezone.format("%Y-%m-%d %H:%M:%S %z").to_string();

println!("Today's date and time in East Eight timezone is {}", date_string);
```

输出将是`Today's date and time in East Eight timezone is 2021-11-05 08:00:00 +0800`。

# 深入探讨

在Rust中，日期和时间被表示为`DateTime<Utc>`类型，这是一个具有时区信息的结构体。这使得转换日期和时间变得更加方便，同时确保时区信息得到正确处理。

当我们将日期转换为字符串时，我们可以使用不同的格式说明符来满足我们的需求。例如，`%Y`表示4位数的年份，`%m`表示月份，`%d`表示日期，`%H`表示24小时制的小时，`%M`表示分钟，`%S`表示秒，`%z`表示时区信息。

# 参考链接

- [Chrono官方文档](https://docs.rs/chrono/)
- [Rust Cookbook: Converting between Datetime and Strings](https://rust-lang-nursery.github.io/rust-cookbook/datetime/parse.html)