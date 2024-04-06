---
date: 2024-01-20 17:31:28.634751-07:00
description: "\u5982\u4F55\u505A\uFF1A \u4EBA\u4EEC\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\
  \u53BB\u7684\u65E5\u671F\u6709\u7740\u60A0\u4E45\u7684\u5386\u53F2\uFF0C\u539F\u59CB\
  \u7684\u65B9\u6CD5\u662F\u4F7F\u7528\u65E5\u5386\u548C\u7B80\u5355\u7684\u8BA1\u6570\
  \u3002\u5728\u8BA1\u7B97\u673A\u65F6\u4EE3\uFF0C\u65E5\u671F\u8BA1\u7B97\u53D8\u5F97\
  \u81EA\u52A8\u5316\u5E76\u4E14\u51C6\u786E\u6027\u5927\u5927\u63D0\u9AD8\u3002Haskell\u4E2D\
  \uFF0C`Data.Time`\u5E93\u662F\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684\u5F3A\
  \u5927\u5DE5\u5177\u3002\u9664\u4E86`Data.Time`\uFF0C\u8FD8\u6709\u8BF8\u5982`Time`\u548C\
  `old-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.992182-06:00'
model: gpt-4-1106-preview
summary: "`addDays`\u51FD\u6570\u63A5\u53D7\u4E24\u4E2A\u53C2\u6570\uFF1A\u5929\u6570\
  \u548C\u65E5\u671F\u3002\u5929\u6570\u53EF\u4EE5\u662F\u6B63\u6570\u6216\u8D1F\u6570\
  \uFF0C\u5206\u522B\u8868\u793A\u672A\u6765\u6216\u8FC7\u53BB\u3002\u5B83\u8FD4\u56DE\
  \u4E00\u4E2A\u65B0\u7684`Day`\u7C7B\u578B\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
