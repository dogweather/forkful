---
title:                "未来または過去の日付の計算"
html_title:           "Rust: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
計算すること - 何かを特定の日付に移動させたり、未来または過去の日付を計算すること。  なぜプログラマーがそれをするのか - 程よく計算されたプログラムは、正確な日付の計算を行い、タイムスタンプやスケジュール作成などの多くのタスクで重要です。

## How to:
```Rust
use chrono::{DateTime, Utc, duration::Duration};

// calculate a date in the future by adding a specified number of days to today's date
let today = Utc::today();
let future_date = today + Duration::days(30);
println!("The date 30 days from now is: {}", future_date);

// calculate a date in the past by subtracting a specified number of days from today's date
let past_date = today - Duration::days(14);
println!("The date 14 days ago was: {}", past_date);

// calculate a date in the future or past based on a specific date and a specified number of years, months, and days
let specific_date = DateTime::parse_from_rfc2822("Sun, 15 Oct 2017 15:23:01 +0000").unwrap();
let calculated_date = specific_date + Duration::weeks(10) + Duration::days(2);
println!("The date 10 weeks and 2 days from the specified date is: {}", calculated_date);
```

Output:
```
The date 30 days from now is: 2020-03-19 UTC
The date 14 days ago was: 2020-02-15 UTC
The date 10 weeks and 2 days from the specified date is: 2017-12-27T15:23:01Z
```

## Deep Dive:
計算する日付は、TimestampやISO 8601などさまざまな形式で表現されることができます。過去の日付も、過去または未来の日付も計算することができます。この機能は、様々な日付と時間の操作を簡単にするために提供されています。代替手段として、外部ライブラリや日付計算用の独自のアルゴリズムを使用することができます。日付計算は、タイムゾーンなどの複雑な概念を考慮する必要があるため、実装には注意が必要です。

## See Also:
- [Chrono crate documentation](https://docs.rs/chrono/)
- [Date and time in Rust: A deep dive](https://blog.logrocket.com/date-and-time-in-rust-a-deep-dive/)
- [Understanding Chrono, the Date and Time Library for Rust](https://www.freecodecamp.org/news/understanding-chrono-the-date-and-time-library-for-rust/)