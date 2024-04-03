---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:46.537403-07:00
description: "\u5728 Haskell \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\
  \u83B7\u53D6\u7CFB\u7EDF\u7684\u5F53\u524D\u65F6\u95F4\uFF0C\u5E76\u5C06\u5176\u8F6C\
  \u6362\u4E3A\u53EF\u8BFB\u7684\u65E5\u671F\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u57FA\u4E8E\u65E5\u671F\u6267\u884C\u64CD\u4F5C\uFF0C\
  \u4F8B\u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u8C03\u5EA6\u4EFB\u52A1\u6216\u5728\u5E94\
  \u7528\u7A0B\u5E8F\u4E2D\u7ED9\u4E8B\u4EF6\u52A0\u65F6\u95F4\u6233\u3002"
lastmod: '2024-03-13T22:44:47.827920-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Haskell \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u83B7\
  \u53D6\u7CFB\u7EDF\u7684\u5F53\u524D\u65F6\u95F4\uFF0C\u5E76\u5C06\u5176\u8F6C\u6362\
  \u4E3A\u53EF\u8BFB\u7684\u65E5\u671F\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u57FA\u4E8E\u65E5\u671F\u6267\u884C\u64CD\u4F5C\uFF0C\u4F8B\
  \u5982\u65E5\u5FD7\u8BB0\u5F55\u3001\u8C03\u5EA6\u4EFB\u52A1\u6216\u5728\u5E94\u7528\
  \u7A0B\u5E8F\u4E2D\u7ED9\u4E8B\u4EF6\u52A0\u65F6\u95F4\u6233\u3002."
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
