---
title:                "将日期转换为字符串"
html_title:           "Elm: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在日常开发中，我们经常需要将日期转换为字符串。这可能是为了展示给用户，也可能是为了存储在数据库中。使用 Elm 编程语言可以轻松地将日期转换为字符串，并且具有强大的类型系统可以帮助我们避免一些常见的错误。

## 如何做

首先，我们需要导入 `Date` 模块来处理日期。然后，使用 `format` 函数将日期转换为字符串，该函数需要两个参数：一个格式化字符串和一个日期对象。以下是一个简单的例子：

```Elm
import Date exposing (..)

dateString : String
dateString =
  format "MMM dd, yyyy" (fromCalendarDate 2020 6 18)
```

输入的格式化字符串定义了日期的显示方式，具体规则请参考 [Elm 文档](https://package.elm-lang.org/packages/elm/time/latest/Time#format)。上面的例子输出的字符串将是 `Jun 18, 2020`。我们还可以使用更复杂的日期格式，例如 `MM/dd/yyyy` 或者 `yyyy-MM-dd HH:mm:ss`。

如果想要在字符串中添加具体的时间信息，可以使用 `andTime` 函数：

```Elm
dateString : String
dateString =
  format "MMM dd, yyyy - HH:mm:ss" (andTime (fromCalendarDate 2020 6 18) (fromClockTime 14 30 0))
```

上面的例子输出的字符串将是 `Jun 18, 2020 - 14:30:00`。

## 深入探讨

在 Elm 中，日期和时间分别被抽象为 `Date` 和 `Time` 类型。`Date` 类型表示一个特定的日期，例如 `June 18, 2020`。而 `Time` 类型则表示一个具体的时间，例如 `14:30:00`。通过结合两者，我们可以获得一个完整的日期+时间信息。这样的设计既清晰又灵活，使得处理日期和时间变得非常简单。

## 参考链接

* [Elm 日期和时间文档](https://package.elm-lang.org/packages/elm/time/latest/)
* [更多关于 Elm 的资料](https://elm-lang.org/)