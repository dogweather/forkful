---
title:                "Haskell: 将日期转换为字符串"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，经常会遇到需要将日期转换为字符串的情况。这样做可以帮助开发者更容易地处理日期数据，并将其储存、显示或用作其他操作。在Haskell中，有多种方法可以实现这一功能。

## 怎样做

为了将日期转换为字符串，我们需要使用Haskell中的`Data.Time`模块。首先，我们需要引入该模块并定义一个日期对象，例如今天的日期：

```Haskell
import Data.Time
today :: Day
today = fromGregorian 2020 2 1
```

然后，我们可以使用`formatTime`函数将日期转换为我们想要的格式。下面是一个将日期转换为YYYY-MM-DD格式的例子：

```Haskell
dateToString :: Day -> String
dateToString date = formatTime defaultTimeLocale "%Y-%m-%d" date

dateToString today
-- "2020-02-01"
```

除了YYYY-MM-DD格式，还有很多其他格式可以选择。例如，将日期转换为中文格式：

```Haskell
dateToStringCN :: Day -> String
dateToStringCN date = formatTime (mkChineseLocale (Just "UTF-8")) "%Y年%m月%d日" date

dateToStringCN today
-- "2020年02月01日"
```

更多可用的格式请参考`Data.Time.Format`模块的文档。

## 深入探讨

实际上，`formatTime`函数的类型是`FormatTime`，它定义了可以用来格式化任何类型的时间数据的格式。这意味着我们也可以将时间转换为字符串，而不仅仅是日期。例如，将时间转换为小时：分钟的格式：

```Haskell
timeToString :: TimeOfDay -> String
timeToString time = formatTime defaultTimeLocale "%H:%M" time

timeToString (TimeOfDay 15 30 0)
-- "15:30"
```

除了日期和时间，我们还可以将`UTCTime`（格林威治标准时间）转换为字符串，或者使用自定义的日期和时间类型。不管是什么类型，`formatTime`函数都可以帮助我们将时间转换为任何想要的格式。

## 参考链接

- [Data.Time模块文档](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Data.Time.Format模块文档](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)