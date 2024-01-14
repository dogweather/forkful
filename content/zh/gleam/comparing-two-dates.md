---
title:                "Gleam: Please forgive and correct me, if I made mistakes.比较两个日期"
simple_title:         "Please forgive and correct me, if I made mistakes.比较两个日期"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期？

在编程中，经常需要比较两个日期来确定时间先后顺序或者计算时间差。比如在开发电商平台时，需要判断订单生成的先后顺序，来决定发货的优先级。因此，学会如何比较两个日期是非常有用的。今天我们就来探讨一下在 Gleam 中如何比较两个日期。

## 如何比较两个日期？

比较两个日期可以使用标准库中的 `Time` 模块下的 `compare` 函数。该函数接受两个 `Time` 类型的参数，如果第一个日期早于第二个日期，则返回 `-1`，如果两个日期相同，则返回 `0`，如果第一个日期晚于第二个日期，则返回 `1` 。

```Gleam
import Time

let date1 = Time.now()
let date2 = Time.add_days(Time.now(), 2)

let result = Time.compare(date1, date2)

// result 的值为 -1，因为 date1 比 date2 早两天
```

你也可以使用条件表达式来比较两个日期：

```Gleam
if date1 < date2 {
  // do something
} else if date1 == date2 {
  // do something
} else {
  // do something
}
```

还可以利用 `Time` 模块下的 `diff` 函数来计算两个日期之间的时间差。该函数接受两个 `Time` 类型的参数，返回一个 `Time.Delta` 类型的结果，包含了天数、小时数、分钟数和秒数的差值。

```Gleam
import Time

let date1 = Time.parse("2021-05-01 00:00:00", "%Y-%m-%d %H:%M:%S")
let date2 = Time.now()

let diff = Time.diff(date1, date2)

// diff 的值为
// Time.Delta(
//   days: 3,
//   hours: 12,
//   minutes: 30,
//   seconds: 0,
// )
```

## 深入探讨比较两个日期

在 Gleam 中，日期和时间被表示为 `Time` 类型，其内部实际上是一个整数，代表自 UTC 时间 1970 年 1 月 1 日 00:00:00 以来经过的秒数。这种表示方法使得比较两个日期变得简单明了，而且可以方便地进行时间计算。

值得注意的是，Gleam 中的日期不包含时区信息，所以需要在使用时手动转换成本地时区。同时，由于不同的时区可能有不同的夏令时规则，所以在处理跨时区的日期时需要格外小心。

## 参考链接

- [官方标准库文档 - Time 模块](https://gleam.run/core/time.html)
- [Gleam 语言文档 - 日期和时间](https://gleam.run/book/timers-and-time.html)
- [Gleam 标准库源码 - Time 模块](https://github.com/gleam-lang/std/blob/master/lib/core/Time.gleam)

# 参见

- [在 Gleam 中获取当前时间](https://example.com)
- [如何在 Gleam 中格式化日期和时间](https://example.com)
- [处理跨时区日期的注意事项](https://example.com)