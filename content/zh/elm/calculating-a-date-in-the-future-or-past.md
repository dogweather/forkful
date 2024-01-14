---
title:    "Elm: 计算未来或过去的日期"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##为什么

在编程世界中，我们经常需要计算未来或过去的日期。这可能是为了创建日历应用程序，或者是为了预测特定日期的发生事件。无论原因如何，学习如何用Elm编程语言计算日期是一个有用的技能。

##如何做

要计算未来或过去的日期，我们需要使用一些预定义的函数和数据类型。首先，我们需要导入Date模块：

```Elm
import Date exposing (..)
```

接下来，我们可以用以下函数来创建一个日期：

```Elm
date : Int -> Int -> Int -> Date
```

这个函数接受年、月、日作为输入，并返回一个日期对象。例如，要创建2021年5月10日的日期，我们可以这样写：

```Elm
date 2021 5 10
```

除此之外，我们还可以使用以下函数来计算未来或过去的日期：

```Elm
add : Time -> Date -> Date
```

这个函数接受一个时间间隔和一个日期作为输入，并返回一个在指定时间间隔后的日期。例如，要计算2021年5月10日之后100天的日期，我们可以这样写：

```Elm
add (days 100) (date 2021 5 10)
```

类似地，我们还可以使用以下函数来计算过去的日期：

```Elm
sub : Time -> Date -> Date
```

这个函数接受一个时间间隔和一个日期作为输入，并返回一个在指定时间间隔前的日期。例如，要计算2021年5月10日之前100天的日期，我们可以这样写：

```Elm
sub (days 100) (date 2021 5 10)
```

##深入探讨

对于日期计算，我们可以使用不同的时间单位，如天、月、年、秒等。Elm提供了各种函数来处理日期和时间，使得在计算中更加便捷。

相比其他语言，Elm的日期和时间模块具有强大的类型安全性，因此可以减少一些常见的错误。此外，它也可以与其他模块结合使用，使得在实际项目中更加方便。

##另请参阅

- Elm官方文档：https://elm-lang.org/docs 
- Elm日期和时间模块：https://package.elm-lang.org/packages/elm/time/latest/