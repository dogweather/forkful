---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:46.537403-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Haskell \u7684\u6807\u51C6\u5E93 `base`\
  \ \u63D0\u4F9B\u4E86 `Data.Time` \u6A21\u5757\uFF0C\u8BE5\u6A21\u5757\u63D0\u4F9B\
  \u4E86\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684\u529F\u80FD\u3002\u8FD9\u91CC\
  \u5C55\u793A\u4E86\u5982\u4F55\u4F7F\u7528\u5B83\u6765\u83B7\u53D6\u5F53\u524D\u65E5\
  \u671F\uFF1A."
lastmod: '2024-03-13T22:44:47.827920-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u7684\u6807\u51C6\u5E93 `base` \u63D0\u4F9B\u4E86 `Data.Time` \u6A21\
  \u5757\uFF0C\u8BE5\u6A21\u5757\u63D0\u4F9B\u4E86\u5904\u7406\u65E5\u671F\u548C\u65F6\
  \u95F4\u7684\u529F\u80FD\u3002\u8FD9\u91CC\u5C55\u793A\u4E86\u5982\u4F55\u4F7F\u7528\
  \u5B83\u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
