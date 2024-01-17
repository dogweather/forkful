---
title:                "计算未来或者过去的日期"
html_title:           "Elm: 计算未来或者过去的日期"
simple_title:         "计算未来或者过去的日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 什么 和 为什么？
计算未来或过去日期是指根据给定的时间和时间差，计算出一个新的日期。程序员经常需要做这样的计算，以便在他们的应用程序中预测日期或跟踪时间。

# 怎么做：
### 例子：
Elm 提供了一个 Date 模块，具有有用的函数来处理日期。我们来看一个简单的例子，假设我们要计算当前日期的三个月后的日期：

```Elm
import Date exposing (..)

futureDate : Date
futureDate = add (months 3) date

main = 
  date |> toString
  "当前日期：" |> append
  |> toString
  (toString futureDate) |> append
  |> toString
  |> Html.text

/*** 输出：当前日期：2019-07-18, 新的日期：2019-10-18 ***/
```

我们首先导入 Date 模块，并使用 `add` 函数来添加时间差。在这个例子中，我们传入 `months 3`，表示三个月的时间差。然后我们使用 `toString` 函数将日期转换为字符串，并使用 `append` 函数将字符串连接起来。最后，我们使用 `Html.text` 函数将结果输出到 HTML 页面。

另一个例子，假设我们要计算昨天的日期：

```Elm
yesterday : Date
yesterday = sub (days 1) date

main = 
  date |> toString
  "当前日期：" |> append
  |> toString
  (toString yesterday) |> append
  |> toString
  |> Html.text

/*** 输出：当前日期：2019-07-18, 昨天的日期：2019-07-17 ***/
```

在这个例子中，我们使用 `sub` 函数来减去一个时间差，即一天。其他的时间差函数还包括 `years`、`weeks`、`hours`等，可以根据具体需求来选择使用哪一个。

# 深入了解：
计算日期的技术已经存在很长一段时间了，并且在不同的编程语言中都有不同的实现。除了使用 Elm 的 Date 模块，还可以通过其他方式来计算日期，比如使用第三方库或手动编写算法。这些方法各有优缺点，可以根据项目的需要来选择。

在实现上，Elm 使用了一种叫做"函数式编程"的方法来处理日期。这种方法强调将代码分离为独立的函数，并尽可能避免副作用。这使得代码更加清晰、可读性更高，并且不易出错。

# 参考链接：
- [Elm Date 模块文档](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [函数式编程概念](https://medium.com/@opensourcemeath/functions-and-benefits-of-functional-programming-407fd4b8c5a0)
- [手动计算日期的算法](https://www.timeanddate.com/date/duration.html)