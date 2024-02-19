---
aliases:
- /zh/haskell/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:28.634751-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u67D0\u5929\u4E4B\u524D\u6216\u4E4B\u540E\u7684\u5177\u4F53\u65E5\u671F\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u4E8B\u4EF6\
  \u9884\u5B9A\u3001\u5230\u671F\u63D0\u9192\u6216\u5386\u53F2\u6570\u636E\u5206\u6790\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.190118
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u786E\u5B9A\u67D0\u5929\u4E4B\u524D\u6216\u4E4B\u540E\u7684\u5177\u4F53\u65E5\u671F\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u4E8B\u4EF6\
  \u9884\u5B9A\u3001\u5230\u671F\u63D0\u9192\u6216\u5386\u53F2\u6570\u636E\u5206\u6790\
  \u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么和为什么？
计算未来或过去的日期就是确定某天之前或之后的具体日期。程序员这么做是为了处理事件预定、到期提醒或历史数据分析。

## 如何做：
```Haskell
import Data.Time

-- 计算三天后的日期
threeDaysLater :: IO Day
threeDaysLater = do
  today <- utctDay <$> getCurrentTime
  return $ addDays 3 today

-- 计算三天前的日期
threeDaysBefore :: IO Day
threeDaysBefore = do
  today <- utctDay <$> getCurrentTime
  return $ addDays (-3) today  

-- 示例输出
main :: IO ()
main = do
  putStrLn "三天后的日期:"
  threeDaysLater >>= print
  putStrLn "三天前的日期:"
  threeDaysBefore >>= print
```

## 深入了解
人们计算未来或过去的日期有着悠久的历史，原始的方法是使用日历和简单的计数。在计算机时代，日期计算变得自动化并且准确性大大提高。Haskell中，`Data.Time`库是处理日期和时间的强大工具。除了`Data.Time`，还有诸如`Time`和`old-time`等替代品。`Data.Time`提供了一组功能，比如解析日期、时间加减、时区处理等。

`addDays`函数接受两个参数：天数和日期。天数可以是正数或负数，分别表示未来或过去。它返回一个新的`Day`类型。

另外，Haskell的惰性计算特性和强类型系统为日期时间处理提供了额外的灵活性和安全性。使用`IO`类型与外部世界交互是必需的，因为当前日期和时间依赖于执行程序的时刻。

## 参见
- Haskell `Data.Time`模块文档: https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html
- Haskell时间库比较: https://wiki.haskell.org/Library/Time
- 实践Haskell日期和时间编程: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-dates-and-times-in-haskell
