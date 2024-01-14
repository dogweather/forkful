---
title:                "Haskell: 比较两个日期。"
simple_title:         "比较两个日期。"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期？

在编程过程中，我们经常需要比较不同的日期，例如根据日期筛选数据或者计算日期之间的差值。比较两个日期可以帮助我们更有效地处理这些任务。

## 如何比较日期？

在Haskell中，比较两个日期最常用的方法是使用`Data.Time`库中的`Day`类型。让我们来看一个简单的例子：

```Haskell
import Data.Time (Day, fromGregorian, diffDays)

-- 创建两个日期
firstDate :: Day
firstDate = fromGregorian 2020 1 1

secondDate :: Day
secondDate = fromGregorian 2021 1 1

-- 计算两个日期之间的天数差
daysDiff :: Integer
daysDiff = diffDays secondDate firstDate

-- 打印结果
main = print daysDiff
-- 输出：365
```

在这个例子中，我们使用`fromGregorian`函数创建了两个日期，然后使用`diffDays`函数计算了它们之间的天数差。将`Day`类型的日期作为参数传入这些函数即可。对于更复杂的日期比较，您可以使用`Data.Time`库中提供的其他函数来实现。

## 深入了解日期比较

有时候，我们需要比较日期的精确时间，而不仅仅是日期。在Haskell中，我们可以使用`Data.Time.Clock`库中的`UTCTime`类型来比较日期和时间。让我们修改上面的例子，并加入具体的时间：

```Haskell
import Data.Time.Clock (UTCTime, UTCTime (UTCTime), diffUTCTime)

-- 创建两个精确时间
firstTime :: UTCTime
firstTime = UTCTime firstDate (8 * 3600)

secondTime :: UTCTime
secondTime = UTCTime secondDate (12 * 3600)

-- 计算两个时间之间的秒数差
secondsDiff :: NominalDiffTime
secondsDiff = diffUTCTime secondTime firstTime

-- 打印结果
main = print secondsDiff
-- 输出：14400
```

在这个例子中，我们使用`UTCTime`类型来表示具体的日期和时间，并使用`diffUTCTime`函数计算了它们之间的秒数差。

# 参考链接

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Data.Time库文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Data.Time.Clock库文档](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)