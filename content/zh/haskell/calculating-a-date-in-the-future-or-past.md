---
title:                "计算过去或未来的日期。"
html_title:           "Haskell: 计算过去或未来的日期。"
simple_title:         "计算过去或未来的日期。"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么
有时候我们需要计算过去或未来的日期，比如计算明天是几号，或者在几天后的日期。使用Haskell可以让我们轻松地进行这样的日期计算。

## 如何
要在Haskell中计算日期，我们可以使用"Data.Time"库中的函数。首先，我们需要导入这个库：

```Haskell
import Data.Time
```

接下来，我们可以使用```UTCTime```和```addDays```函数来计算过去或未来的日期。比如，我们可以计算今天是几号：

```Haskell
getCurrentTime >>= print . utctDay
```

输出结果可能是："2020-04-01"，说明今天是4月1日。如果我们想计算明天的日期，可以使用```addDays```函数，传入1作为参数：

```Haskell
addDays 1 <$> getCurrentTime >>= print . utctDay
```

输出结果可能是："2020-04-02"，代表明天的日期。同理，如果我们想计算过去的日期，可以传入一个负数作为参数。比如，如果我们想计算5天前的日期，可以这样写：

```Haskell
addDays (-5) <$> getCurrentTime >>= print . utctDay
```

输出结果可能是："2020-03-27"，是5天前的日期。

## 深入探讨
除了```addDays```函数，"Data.Time"库还提供了其他函数来进行更复杂的日期计算，比如```addGregorianMonthsClip```、```addGregorianYearsClip```等。使用这些函数可以让我们更灵活地进行日期计算。同时，在处理日期的时候还要注意时区、闰年等因素，这些都可以通过该库中的函数来处理。

## 参考
- [Haskell官方文档](https://www.haskell.org/)
- [Data.Time文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)