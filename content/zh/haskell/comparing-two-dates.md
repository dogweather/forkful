---
title:    "Haskell: 比较两个日期"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较日期？
日期是计算机程序中经常用到的数据类型。比较两个日期可以帮助我们在编程时更轻松地处理时间和日期相关的任务，例如计算两个日期之间的时间差或者检查一个日期是否在另一个日期的范围内。

## 如何比较日期
在Haskell中，我们可以使用标准库中的 `Data.Time` 模块来处理日期。首先，我们需要导入这个模块：
```Haskell
import Data.Time
```
比较日期的核心函数是 `diffDays`，它可以计算两个日期之间相差的天数。例如，我们想要比较今天和明天的日期：
```Haskell
let today = utctDay $ getCurrentTime
let tomorrow = UTCTime (addDays 1 $ utctDay today) (secondsToDiffTime 0)
let diff = diffDays tomorrow today
```
`utctDay` 函数可以从 `getCurrentTime` 函数的返回值中提取出日期部分。然后，我们使用 `addDays` 函数和 `secondsToDiffTime` 函数来创建明天的日期。最后，我们将两个日期传递给 `diffDays` 函数，得到它们之间相差的天数。

想要判断一个日期是否在另一个日期的范围内，我们可以使用 `withinDays` 函数。它接受三个参数：待检查的日期、起始日期和结束日期。如果待检查的日期在起始日期和结束日期之间，则返回 `True`，否则返回 `False`。

```Haskell
withinDays :: Day -> Day -> Day -> Bool
withinDays = isBetweenInclusive

today `withinDays` (addDays 3 today) (addDays 7 today) -- 返回 True
```
在这个例子中，我们将 `withinDays` 函数和 `addDays` 函数一起使用来检查今天是否在未来一周内。

## 深入了解比较日期
Haskell中的日期类型是 `Day`，它实际上是一个整数，用于表示自公元0年1月1日以来经过的天数。这种表示方法的好处是它是唯一的、不可变的和便于比较的。因此，我们可以使用普通的数学运算符来处理日期。

一个常见的挑战是如何比较带有时区信息的日期。例如，我们想比较两个不同时区的时间戳。在这种情况下，我们可以使用 `UTCTime` 数据类型，它是由日期加上一个时间差构成的。使用 `utctDayTime` 函数，我们可以从 `UTCTime` 类型获取日期和时间。比较 `UTCTime` 类型的值可以使用 `compare` 函数。

另一个挑战是如何比较日期和时间。在这种情况下，我们可以使用 `diffUTCTime` 函数来计算两个时间戳之间相差的秒数，然后使用 `diffTimeOfDay` 函数来获取时间部分。

## 查看也可
- [Haskell日期文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [带时区信息的日期比较](https://stackoverflow.com/questions/3836743/comparing-utc-times-with-different-time-zones-in-haskell)
- [日期和时间的比较](https://stackoverflow.com/questions/18335137/compare-date-and-time-in-haskell)