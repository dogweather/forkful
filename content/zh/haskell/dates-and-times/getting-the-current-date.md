---
title:                "获取当前日期"
aliases: - /zh/haskell/getting-the-current-date.md
date:                  2024-02-03T19:09:46.537403-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Haskell 中获取当前日期涉及获取系统的当前时间，并将其转换为可读的日期格式。程序员这样做是为了基于日期执行操作，例如日志记录、调度任务或在应用程序中给事件加时间戳。

## 如何操作：
Haskell 的标准库 `base` 提供了 `Data.Time` 模块，该模块提供了处理日期和时间的功能。这里展示了如何使用它来获取当前日期：

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

示例输出：
```
2023-04-12
```

为了更多的灵活性，比如格式化日期或处理不同的时区，`time` 库非常宝贵。这里是如何格式化当前日期的方法：

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

这将打印出调整为本地时区的当前日期，格式为 `YYYY-MM-DD`。

另外，对于第三方库的支持，`time` 非常受 Haskell 社区的推荐和使用，因为它拥有广泛的日期和时间操作能力。上述示例就利用了这个库。

如果您需要更全面的日期操作，包括从字符串解析或与日期和时间进行算术操作，探索 `Data.Time` 内的额外功能将会非常有益。
