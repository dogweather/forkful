---
title:                "计算过去或未来的日期"
html_title:           "Elm: 计算过去或未来的日期"
simple_title:         "计算过去或未来的日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

为什么：为了让我们的应用程序或网站更加灵活和智能，有时候需要计算将来或过去的日期。例如，在预订机票或制定日程时，计算未来或过去的日期是非常重要的。

如何：在 Elm 中，通过使用 ``Elm/time`` 模块可以轻松进行日期计算。下面的代码示例演示如何使用模块中的函数来计算未来或过去的日期，你可以根据自己的需求来进行调整。

```
Elm/time
    |> Time.now
    |> Time.inUtc
    |> Time.toIsoString
    |> Debug.log "Current date"
```

上面的代码将打印出当前的日期，如“2021-07-20T15:15:00.000Z”。接下来，我们可以使用 ``Time.add`` 函数来计算将来或过去的日期。

```
Elm/time
    |> Time.now
    |> Time.inUtc
    |> Time.add Time.day 7
    |> Time.toIsoString
    |> Debug.log "Date 1 week from now"
```

上面的代码将打印出一周后的日期，如“2021-07-27T15:15:00.000Z”。同时，我们也可以使用 ``Time.sub`` 函数来计算过去的日期。

```
Elm/time
    |> Time.now
    |> Time.inUtc
    |> Time.sub Time.month 1
    |> Time.toIsoString
    |> Debug.log "Date 1 month ago"
```

上面的代码将打印出一个月前的日期，如“2021-06-20T15:15:00.000Z”。

深入探讨：在日期计算中，我们需要特别注意时区的问题。在 Elm 中，可以使用 ``Time.inZone`` 函数来将日期转换为指定时区的时间。另外，还可以使用 ``Time.compare`` 函数来比较两个日期的先后顺序。

另外，我们也可以通过使用 ``Time.fromIsoString`` 函数来将日期字符串转换为日期类型，以便进行更复杂的计算。

参考链接：

- ``Elm/time`` 模块文档：https://package.elm-lang.org/packages/elm/time/latest/
- Elm 中文文档：https://guide.elm-lang.org/