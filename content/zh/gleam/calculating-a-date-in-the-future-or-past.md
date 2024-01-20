---
title:                "计算未来或过去的日期"
html_title:           "Gleam: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 是什么与为什么？

计算未来或过去的日期是指在当前日期基础上加减特定的时间单位，比如天、周、月、年等。程序员经常需要进行此类计算来处理和时间相关的需求，例如制定日程、预设提醒、创建日期日志等。

## 如何完成：

使用 Gleam 进行日期计算非常直观，下面是一个计算5天后日期的代码示例：

```gleam
import gleam/datetime.{Date}
import gleam/int

fn days_later(date: Date, days: Int) -> Date {
    Date.add_days(date, days)
}

fn main() {
    let now = Date.today()
    let future_date = days_later(now, 5)
    future_date
}
```
运行结果就是5天后的日期。

## 深入解析

历史上，人们处理日期时间的方式各异，直到 1972 年，由于计算机科技的发展，导致我们现在的日期时间处理方案显得更为科学合理。 

曾经没有标准库支持，开发人员不得不针对特定的编程语言自己写代码来进行日期计算。然而，这样做通常会导致一些奇怪的问题，例如无法正确处理闰年或跨年问题。

Gleam 的 `gleam/datetime` 模块提供了一种高级且标准的方式来处理日期计算。它对不同的编程语言提供了简洁一致的 API，并提供了大量的实用方法来操作和处理日期和时间。

然而，你也可以选择其它库或自行实现相关的功能，但如果非要在已经有很好的轮子的情况下，还要重复地发明轮子，未免有些浪费时间。

## 参考资料

* 日期计算相关的知识: [https://www.timeanddate.com/date/workdays.html](https://www.timeanddate.com/date/workdays.html)