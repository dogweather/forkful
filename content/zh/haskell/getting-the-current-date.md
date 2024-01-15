---
title:                "获取当前日期"
html_title:           "Haskell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是编写复杂程序或应用程序时的一项基本任务。Haskell提供了简单且强大的方法来获取当前日期，以便在程序中使用。

## 如何

```Haskell
import Data.Time.Clock
import Data.Time.Format

main = do
    now <- getCurrentTime
    printf "当前时间为：%s" (formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" now)
```

运行代码后，将输出类似于 `当前时间为：18/06/2021 13:24:45` 的结果，其中日期和时间是根据当地时区和语言格式化的。

## 深入探讨

获取当前日期的基本方法是使用 `Data.Time.Clock` 和 `Data.Time.Format` 模块中的 `getCurrentTime` 和 `formatTime` 函数。`getCurrentTime` 返回系统的当前时间，以UTC格式表示，而 `formatTime` 函数可以将时间格式化为字符串，如前面示例中所使用的格式。

另一个有用的函数是 `getCurrentTimeZone`，它返回当前系统的本地时区。这可以避免使用 `getCurrentTime` 返回的UTC时间与本地时区相差的问题。

但是，在某些情况下，格式化字符串中的日期和时间的顺序取决于语言设置。如果你的系统语言设置中的日期顺序与预期的不同，可以使用 `defaultTimeLocale` 更改它。

另外，还有其他有用的日期和时间处理函数，如计算时间差，比较不同的时间等。你可以在官方的[Haskell文档](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)中查找更多信息。

## 参考链接

- [Haskell官方文档：Data.Time.Clock模块](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
- [Haskell官方文档：Data.Time.Format模块](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)