---
title:    "Haskell: 获取当前日期。"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 为什么 
获取当前日期是编程中常见的操作，它可以帮助我们记录程序运行的时间和日期，以及生成带有时间戳的日志文件。在Haskell中，我们可以通过一些简单的代码来获取当前日期，让我们来看看如何实现吧！

## 怎么做
```Haskell
import Data.Time.Clock
import Data.Time.Calendar

-- 获取当前日期和时间
getCurrentDateTime :: IO (UTCTime)
getCurrentDateTime = getCurrentTime

-- 解析日期和时间数据
getDate :: UTCTime -> Day
getTime :: UTCTime -> TimeOfDay

-- 获取当前年份
getCurrentYear :: Day -> Integer
getCurrentYear day = let (year, _, _) = toGregorian day in year

-- 获取当前月份
getCurrentMonth :: Day -> Int
getCurrentMonth day = let (_, month, _) = toGregorian day in month

-- 获取当前日期
getCurrentDay :: Day -> Int
getCurrentDay day = let (_, _, day) = toGregorian day in day

-- 获取当前小时
getCurrentHour :: TimeOfDay -> Int
getCurrentHour time = todHour time

-- 获取当前分钟
getCurrentMinute :: TimeOfDay -> Int
getCurrentMinute time = todMin time

-- 获取当前秒数
getCurrentSecond :: TimeOfDay -> (Int, Pico)
getCurrentSecond time = (todSec time, todSec)
```

#### 输出示例：
```
2021年7月1日
1点30分5秒 (1,30,5)
```

## 深入探究
获取当前日期涉及到了Haskell中两个重要的模块：`Data.Time.Clock`和`Data.Time.Calendar`。其中，`Data.Time.Clock`模块包含了有关时间的函数，例如`getCurrentTime`用来获取当前时间，而`Data.Time.Calendar`模块则包含了与日期有关的函数，例如`toGregorian`用来将`Day`类型的数据转换为年、月、日的元组数据。通过使用这些函数，我们可以轻松地获取当前日期的详细信息。

## 参考链接
- [Haskell时间函数文档](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Clock.html)
- [Haskell日期函数文档](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Calendar.html)
- [Haskell官方文档](https://www.haskell.org/documentation/)