---
title:                "Haskell: 获取当前日期"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么：获取当前日期的原因

在编程中，我们经常需要使用当前的日期来进行各种操作，比如生成文件名、记录日志等。Haskell提供了一种简便的方法来获取当前日期，让我们来看一下如何实现吧！

# 如何使用Haskell获取当前日期

我们可以使用标准库中的`Data.Time`来获取当前日期。首先，让我们导入该模块：

```Haskell
import Data.Time
```

然后，我们可以使用`getCurrentTime`函数来获取当前的日期和时间：

```Haskell
getCurrentTime :: IO UTCTime
```

请注意，`getCurrentTime`函数会返回一个`IO`类型的值，这意味着它在执行时会进行副作用，我们可以使用`<-`操作符来获取其返回的值：

```Haskell
current <- getCurrentTime
```

接下来，我们可以使用`utctDay`函数来获取日期，并使用`show`函数来将日期转换成字符串：

```Haskell
let today = utctDay current
let dateString = show today
```

现在，我们可以将日期打印出来，查看结果：

```Haskell
putStrLn dateString
```

运行程序，你会得到类似于`2021-09-15`的日期字符串。太简单了吧！

# 深入了解获取当前日期

在Haskell中，日期被表示为`Day`类型，它是一个整数，表示自公元前4713年1月1日以来的天数。我们可以使用`toGregorian`函数将`Day`类型的日期转换成`(year, month, day)`的元组，让我们来试试看：

```Haskell
let (year, month, day) = toGregorian today
putStrLn $ show year ++ "年" ++ show month ++ "月" ++ show day ++ "日"
```

输出会变成类似于`2021年9月15日`的字符串。如果你想要获取当前的时间，你可以使用`utctDayTime`函数，它会返回一个`DiffTime`类型的值，表示从午夜以来的秒数。你可以使用`show`函数将其转换为字符串，类似于前面的例子。

现在，你已经学会如何获取当前日期和时间了！如果你想要更多了解日期和时间的相关操作，你可以查看Haskell的`Data.Time`文档。祝你在编程中使用日期顺利～

# 同样查阅

- [Haskell `Data.Time`文档](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell官方网站](https://www.haskell.org/)