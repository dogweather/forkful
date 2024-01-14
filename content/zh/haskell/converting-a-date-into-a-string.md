---
title:                "Haskell: 将日期转换成字符串"
simple_title:         "将日期转换成字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要将日期转换成字符串，例如在打印日志或生成报表时。这可以让我们更容易地阅读和理解日期，同时也方便了数据的处理和比较。

## 如何实现

在Haskell中，我们可以使用Data.Time库来进行日期和时间的处理。具体的转换方法如下所示：

```Haskell
-- 导入Data.Time库
import Data.Time

-- 使用getCurrentTime函数获取当前日期和时间
currentTime :: IO UTCTime
currentTime = getCurrentTime

-- 使用formatTime函数将日期转换成指定的格式字符串
formatTime :: FormatTime t => TimeLocale -> String -> t -> String

-- 示例：将当前日期转换成 “年-月-日” 的格式
main :: IO ()
main = do
  now <- currentTime
  let dateString = formatTime defaultTimeLocale "%Y-%m-%d" now
  putStrLn dateString
```

输出：

```
2021-03-25
```

在上面的例子中，我们使用了`"%Y-%m-%d"`作为格式字符串，其中`%Y`代表4位数的年份，`%m`代表2位数的月份，`%d`代表2位数的日子。除了这个例子，还有许多其他的日期格式供我们选择，详情可以参考[官方文档](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)。

## 深入探索

在Haskell中，Date和Time的处理都是由`UTCTime`类型来表示的。它的内部结构其实是一个浮点数，表示从格林威治时间1970年1月1日零点到当前时间的秒数。因此，在进行日期转换时，我们其实是将这个浮点数转换成我们需要的格式来展示。具体的实现细节可以参考[这篇文章](http://www.haskellforall.com/2021/01/converting-datetimes-into-strings-in.html)。

## 参考链接

- [Haskell官方文档](https://www.haskell.org/)
- [Data.Time库文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [日期和时间格式参考](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [博客文章：Converting Datetimes into Strings in Haskell](http://www.haskellforall.com/2021/01/converting-datetimes-into-strings-in.html)