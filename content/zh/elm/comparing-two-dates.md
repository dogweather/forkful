---
title:                "Elm: Please excuse any grammatical mistakes.比较两个日期"
simple_title:         "Please excuse any grammatical mistakes.比较两个日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较日期？

日期比较是在编程中常见的任务之一，它可以用来判断某个事件是否发生在另一个事件之前或之后。在编写网页或应用程序时，我们经常需要比较不同的日期，以便根据不同的日期来展示不同的内容。比如，一个活动网站可能会根据当前日期来展示不同的活动，日期比较就可以帮助我们实现这样的功能。下面我们来看看如何使用 Elm 来比较日期。

## 如何进行日期比较

Elm 提供了一个 `Date` 模块来处理日期相关的操作。要比较两个日期，我们首先需要定义两个日期变量，然后使用 `Date.compare` 函数来比较它们。下面是一个代码示例：

```Elm
import Date exposing (compare)
import Time exposing (Posix)

oldDate : Posix
oldDate = Time.millisToPosix 1600000000

newDate : Posix
newDate = Time.millisToPosix 1700000000

result : Ordering
result = Date.compare oldDate newDate
```

首先我们导入了 `Date` 和 `Time` 模块，并定义了两个变量 `oldDate` 和 `newDate`，它们分别表示两个日期。接下来，我们使用 `Time.millisToPosix` 函数来将一个时间戳转换成 `Posix` 类型的日期变量。然后，我们调用 `Date.compare` 函数，将 `oldDate` 和 `newDate` 作为参数传入，它会返回一个 `Ordering` 类型的结果，表示两个日期的相对顺序。最后，我们可以根据 `result` 变量的值来判断哪个日期更早、更晚或相等。

## 深入了解日期比较

在 Elm 中，日期被表示为 `Posix` 类型的数字，它表示自 1970 年 1 月 1 日 0 点起的毫秒数。因此，我们可以通过比较数值大小来判断两个日期的相对顺序。同时，Elm 还提供了 `Date.fromString` 函数来将一个字符串转换成日期变量，这样我们就可以直接根据字符串来比较日期。

此外，对于复杂的日期比较需求，我们还可以使用 `Date.toYearMonth` 和 `Date.toHour` 等函数来获取日期的年月和小时信息，并根据它们进行比较。

# 看看其他教程

如果你想了解更多有关日期比较的信息，可以查阅 Elm 官方文档中的 `Date` 模块部分。此外，你还可以参考下面的链接获取更多 Elm 相关的资源：

- [Elm 官方文档](https://elm-lang.org/docs/)
- [Awesome Elm （非官方）精选列表](https://github.com/sporto/awesome-elm)