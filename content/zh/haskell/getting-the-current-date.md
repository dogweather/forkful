---
title:    "Haskell: 获取当前日期"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

今天我们来介绍如何在Haskell中获取当前日期。不管你是新手还是有经验的程序员，获取当前日期是一个非常有用的技能。不仅可以帮助你记录程序运行的时间，还可以帮助你创建时间相关的功能，比如日历应用程序。

## 为什么

获取当前日期是一个常见的编程需求。无论你是在计算机科学、数据科学还是Web开发领域工作，都可能需要使用当前日期作为程序的一部分。因此，了解如何获取当前日期是非常重要的。

## 如何操作

在Haskell中，获取当前日期最常用的方法是使用Data.Time库。让我们来看一个简单的代码例子：

```Haskell
import Data.Time

--获取当前日期
currentDate :: IO String
currentDate = do
   time <- getCurrentTime
   let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d" time
   return formattedTime

--输出结果：2021-07-13
main :: IO ()
main = do
   date <- currentDate
   putStrLn $ "今天是：" ++ date
```

在上面的代码中，我们首先导入了Data.Time库，然后创建了一个名为currentDate的函数，它返回一个IO String。该函数使用getCurrentTime函数从系统中获取当前时间，并使用formatTime函数将时间格式化为YYYY-MM-DD的形式。最后，我们在main函数中调用currentDate函数，并通过putStrLn函数将结果打印出来。

## 深入探讨

除了上面介绍的方法，Haskell还提供了其他许多获取日期的函数和数据类型。比如，可以使用TimeZone来表示时区，使用CalendarTime来表示具体的日期和时间，而不仅仅是日期。

另外，Data.Time库中还包含了一些方便的函数，比如添加或减少指定的时间间隔，演示某个日期是否在某个时间范围内等等。

## 参考资料

1. [Haskell Data.Time模块文档](https://hackage.haskell.org/package/time-1.8/docs/Data-Time.html)
2. [Haskellers论坛上有关获取当前日期的讨论](https://www.haskell.org/pipermail/haskell-cafe/2012-May/101147.html)
3. [Haskell Wiki上关于日期和时间处理的说明](https://wiki.haskell.org/Date_and_time)

## 参考阅读

1. [Haskell for Beginners：入门教程](https://www.cs.nott.ac.uk/~pszgmh/teaching/haskell98.pdf)
2. [Learn You a Haskell for Great Good!：Haskell入门指南](http://learnyouahaskell.com/)
3. [Real World Haskell：实践指南](http://book.realworldhaskell.org/read/)