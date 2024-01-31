---
title:                "获取当前日期"
date:                  2024-01-20T15:14:47.843346-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
获取当前日期是读取操作系统提供的日期和时间。程序员这样做为了记录事件、处理过期数据或显示动态信息。

## How to: (如何操作：)
```Haskell
import Data.Time

main :: IO ()
main = do
  currentDate <- getCurrentTime
  print $ utctDay currentDate
```

示例输出：
```
2023-04-07
```

## Deep Dive (深入探究)
Haskell的`Data.Time`库，特别是`getCurrentTime`函数，是获取当前日期和时间的主流方法。它是基于世界时（UTC）的，确保标准一致性。不过，如果你需要特定时区的日期，可能得使用`Data.Time.LocalTime`的相关函数。历史上，Haskell发展了多个处理日期和时间的库，但`Data.Time`由于其全面性和相对直接的接口，已成为事实上的标准。

## See Also (参见)
- Haskell `Data.Time`库：http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Haskell `Data.Time.LocalTime`：http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html
- 关于UTC的解释：https://en.wikipedia.org/wiki/Coordinated_Universal_Time
