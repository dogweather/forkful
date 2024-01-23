---
title:                "计算未来或过去的日期"
date:                  2024-01-20T17:30:51.994754-07:00
model:                 gpt-4-1106-preview
simple_title:         "计算未来或过去的日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期，就是找出从现在开始的某个时间点相距多少天的日期。程序员做这个通常是为了事件规划、提醒功能或者数据分析。

## 如何做：
```gleam
import gleam/calendar.{Date, add_days}

pub fn main() {
  let today = Date(year: 2023, month: 4, day: 1)
  
  // 计算10天后的日期
  let future_date = add_days(today, 10)
  io.println(future_date)

  // 计算10天前的日期
  let past_date = add_days(today, -10)
  io.println(past_date)
}
```
输出：
```
Date(year: 2023, month: 4, day: 11)
Date(year: 2023, month: 3, day: 22)
```

## 深入了解
历史上，日期计算一直是重要的。在没有计算机的时代，它依赖于天文学和数学。Gleam内置的`calendar`模块让日期计算变得简单。选择Gleam而不是JavaScript、Python等，因为Gleam是静态类型语言，更安全、错误更少。此外，它能轻松处理闰年和不同月份天数的差异。
