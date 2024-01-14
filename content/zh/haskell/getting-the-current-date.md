---
title:                "Haskell: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，获取当前日期是一个常见的需求。无论是记录日志、创建文件名或是实现时间敏感的业务逻辑，都需要使用到当前日期信息。因此，了解如何在Haskell中获取当前日期是非常重要的。

## 如何做

在Haskell中，我们可以使用`Data.Time`模块来获取当前日期。首先，我们需要导入这个模块：

```Haskell
import Data.Time
```

然后，我们可以使用`getCurrentTime`函数来获取当前日期和时间的`UTCTime`类型值。这个函数的返回类型是`IO UTCTime`，所以我们需要使用`do`表达式来获取具体的日期和时间值：

```Haskell
currentTime <- getCurrentTime
```

我们可以使用`formatTime`函数将`UTCTime`类型的值格式化为想要的格式。例如，我们可以将其格式化为年-月-日的形式：

```Haskell
let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
```

最后，我们可以使用`putStrLn`函数来打印出当前日期：

```Haskell
putStrLn formattedDate
```

完整的代码如下：

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
  putStrLn formattedDate
```

运行结果：

```
2021-10-16
```

## 深入了解

Haskell中的`Data.Time`模块实现了一个名为`UTCTime`的类型，代表了世界标准时间（UTC），它是一个世界各地都使用的时间标准。通过使用`UTCTime`类型，我们可以保证在任何时区都能获取到相同的当前日期和时间。

除了`getCurrentTime`函数，`Data.Time`模块还提供了其他一些函数来获取不同格式的日期和时间值，详情可参考[官方文档](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)。

## 参考链接

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Data.Time模块文档](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)