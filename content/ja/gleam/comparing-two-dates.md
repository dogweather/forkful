---
title:                "日付を比較する"
date:                  2024-01-20T17:32:57.737900-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を比較する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？

Comparing two dates is checking which comes first or if they're the same. Programmers compare dates to track events, sort records, and manage schedules.

## How to:
やり方：

Let's dive into code:

```gleam
import gleam/calendar.{Date, compare, new}

fn main() {
  let date1 = new(2023, 4, 1) // Year, Month, Date
  let date2 = new(2023, 4, 15)

  compare(date1, date2) // Outputs: Lt (Less than)
}

main()
```

Sample output:

``` 
Lt  
```

`Lt` means `date1` is before `date2`. If it was `Gt`, `date1` would be after `date2`. `Eq` means equal.

## Deep Dive
深い潜水：

Historically, computers see dates as numbers. Tracking time accurately led to comparing these numbers. In Gleam, dates use the `Date` type. You can compare using `compare` from `gleam/calendar`.

Alternatives include manually comparing year, month, and day, but that's error-prone and cumbersome.

Gleam's standard library is solid. It handles oddities, like leap years, so you don't have to. Use `compare` for readability and reliability.

## See Also
関連する情報源：

For more about dates in Gleam:

Explore other date functionalities in Gleam for well-rounded mastery.