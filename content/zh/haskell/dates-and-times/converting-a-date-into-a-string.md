---
date: 2024-01-20 17:36:38.721194-07:00
description: "\u5982\u4F55\u505A\uFF1A Haskell \u7684 `Data.Time` \u5E93\u63D0\u4F9B\
  \u4E86\u5F3A\u5927\u7684\u65E5\u671F\u548C\u65F6\u95F4\u64CD\u4F5C\u51FD\u6570\u3002\
  \u5728\u5386\u53F2\u4E0A\uFF0C\u8FD9\u4E9B\u529F\u80FD\u7531\u591A\u4E2A\u4E0D\u540C\
  \u7684\u5E93\u63D0\u4F9B\uFF0C\u6700\u7EC8\u5408\u5E76\u4E3A\u73B0\u5728\u7684 `time`\
  \ \u5E93\u3002\u6B64\u5E93\u662F Haskell \u5E73\u53F0\u7684\u4E00\u90E8\u5206\u3002\
  \ \u4F7F\u7528 `formatTime` \u51FD\u6570\u53EF\u4EE5\u7075\u6D3B\u5B9A\u4E49\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u663E\u793A\u683C\u5F0F\u3002\u4F8B\u5982\uFF0C`\"\
  %Y-%m-%d\"`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.038713-06:00'
model: gpt-4-1106-preview
summary: "\u4F7F\u7528 `formatTime` \u51FD\u6570\u53EF\u4EE5\u7075\u6D3B\u5B9A\u4E49\
  \u65E5\u671F\u548C\u65F6\u95F4\u7684\u663E\u793A\u683C\u5F0F\u3002\u4F8B\u5982\uFF0C\
  `\"%Y-%m-%d\"` \u5B9A\u4E49\u4E86\u4E00\u4E2A\u56FD\u9645\u6807\u51C6\u683C\u5F0F\
  \uFF08ISO 8601\uFF09\u3002\u5F53\u7136\uFF0C\u6709\u591A\u79CD\u65B9\u5F0F\u6765\
  \u8868\u793A\u65F6\u95F4\u5B57\u7B26\u4E32\uFF0C\u6BD4\u5982\u53EA\u663E\u793A\u5E74\
  \u4EFD\uFF08`\"%Y\"`\uFF09\u6216\u8005\u6DFB\u52A0\u65F6\u95F4\uFF08`\"%Y-%m-%d\
  \ %H:%M:%S\"`\uFF09\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何做：
```Haskell
import Data.Time

-- 假设我们有一个日期
currentDate :: IO UTCTime
currentDate = getCurrentTime

-- 将日期转换成字符串
dateToString :: UTCTime -> String
dateToString = formatTime defaultTimeLocale "%Y-%m-%d"

-- 使用示例
main :: IO ()
main = do
    date <- currentDate
    putStrLn $ dateToString date
```
假设当前日期是2023年4月10日，输出将会是：
```
2023-04-10
```

## 深入探索
Haskell 的 `Data.Time` 库提供了强大的日期和时间操作函数。在历史上，这些功能由多个不同的库提供，最终合并为现在的 `time` 库。此库是 Haskell 平台的一部分。

使用 `formatTime` 函数可以灵活定义日期和时间的显示格式。例如，`"%Y-%m-%d"` 定义了一个国际标准格式（ISO 8601）。当然，有多种方式来表示时间字符串，比如只显示年份（`"%Y"`）或者添加时间（`"%Y-%m-%d %H:%M:%S"`）。

除了 `time`，还有其他库如 `old-time`，但现在推荐使用 `time` 库，因为它提供了更全面的功能和改进的国际化支持。

在底层实现上，日期和时间的转换通常涉及对时区的处理和历法的转换。在 Haskell 中，`UTCTime`（世界协调时间）是处理时间的通用方式，它是独立于时区变化和夏令时调整的。

## 参考链接
- Haskell `time` 库文档：https://hackage.haskell.org/package/time
- 时间格式化字符串规则：https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:formatTime
- 关于更多的时间和日期库对比，可以参考：https://wiki.haskell.org/Library/Time
