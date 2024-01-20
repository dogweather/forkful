---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
获取当前日期意味着让计算机告诉我们今天是几号。开发人员用这个来跟踪事件，记录日志，或者只是显示日期给用户。

## 办法:
在Haskell中，我们可以使用 `Data.Time.Clock` 和 `Data.Time.Calendar` 库来得到当前日期。先让我们引入这些库。

```Haskell
import Data.Time.Clock
import Data.Time.Calendar
```
然后我们可以写一个函数来获取今天的日期:

```Haskell
getCurrentDate :: IO (Integer, Int, Int) -- Year, Month, Day
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay
```
这个函数先获取当前的UTC时间，然后从中提取出日期，最后转为公历（格里高利历）格式。

运行这个函数会显示当前的年，月，日:

```Haskell
main = do
    (year, month, day) <- getCurrentDate
    putStrLn $ "Today is " ++ show day ++ "." ++ show month ++ "." ++ show year
```
## 深入探究
获取当前日期是一个常见操作。大部分语言都有自己获取当前日期的方法。在Haskell的早期版本中，日期和时间是由操作系统提供的，所以具体实现在不同的平台上会有所不同。现在，Haskell有了一个全新的、跨平台兼容的日期和时间库。

此外，当前日期的实现依赖于你的机器所在的时区，所以在有些情况下，你可能需要吧UTC时间转为本地时间。

## 另请参见
* Haskell的`Data.Time`文档 [Data.Time](https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time.html)
* 了解更多关于时间和日期的知识 [Time and Date](https://www.timeanddate.com/)