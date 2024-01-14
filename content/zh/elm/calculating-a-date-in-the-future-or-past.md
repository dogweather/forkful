---
title:                "Elm: 计算未来或过去的日期"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

在日常的编程过程中，我们常常会遇到需要计算未来或过去日期的情况。例如，我们可能需要计算某个事件距离当前日期多久，或是在给定一个日期，计算出该日期的前一天或后一天是哪一天。此时，使用 Elm 来进行日期计算是一个非常方便的选择。它提供了简洁、可靠的方法来处理日期以及与日期相关的操作。

# 如何使用

为了计算未来或过去的日期，我们需要使用 Elm 的 [Date 模块](https://package.elm-lang.org/packages/elm-lang/core/latest/Date)。它提供了一组函数来处理日期，包括创建日期、解析日期、以及计算日期间的差值等。下面是一个计算未来日期的示例代码：

```elm
import Date exposing (..)

-- 创建日期
let current = fromCalendarDate 2021 10 20

-- 计算未来5天的日期
let future = add (days 5) current
```

在上面的代码中，我们首先使用 `fromCalendarDate` 函数来创建了一个日期，然后使用 `add` 函数来计算出距离当前日期5天后的日期。最后，我们可以通过 `toString` 函数将日期转换成字符串格式，方便输出。

另外，如果我们想要计算过去的日期，只需要将 `add` 函数的参数改为负数即可。例如，`add (days -5) current` 将会计算距离当前日期5天前的日期。

# 深入探讨

在 Date 模块中，还有一些其他有用的函数，例如 `toTime` 函数可以将日期转换成 Unix 时间戳，`dayOfWeek` 函数可以获取日期是星期几，以及 `isBefore` 和 `isAfter` 函数可以比较两个日期的先后顺序等。掌握这些函数的使用方法，可以让我们更加灵活地处理日期计算的需求。

除了 Date 模块外，在 [Time 模块](https://package.elm-lang.org/packages/elm-lang/core/latest/Time) 中也提供了一些与日期相关的函数，例如 `toString` 函数可以将 Date 类型转换成字符串，`fromString` 函数可以将字符串解析成 Date 类型，以及 `posixToMillis` 函数可以将 Unix 时间戳转换成毫秒数等。深入了解这些函数的功能，可以让我们更加高效地处理日期相关的操作。

# 查看更多

* [Date 文档](https://package.elm-lang.org/packages/elm-lang/core/latest/Date)
* [Time 文档](https://package.elm-lang.org/packages/elm-lang/core/latest/Time)
* [Date Calculator 示例代码](https://ellie-app.com/bCvNFwQGWQva1)