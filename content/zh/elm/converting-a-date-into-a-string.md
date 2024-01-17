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

## 什么？为什么？

将日期转换为字符串是将日期数据转换为可读格式的过程。程序员经常这样做是因为他们需要以可读的方式显示日期，而不是作为计算机内部的一串数字。

## 如何：

```Elm
import Time

Date.toString (Time.millisToPosix 1555530699822)
--> "2019-04-17"

Date.toString (Time.millisToPosix 1586229199000)
--> "2020-04-07"
```

## 深入探讨：

日期表示法在历史上有很多不同的变化，比如使用日历系统、时区等等。目前，最常用的日期格式是ISO 8601标准，也就是我们在上面的代码示例中使用的格式。

除了使用 `Date.toString` 函数之外，还可以使用 `Date.fromCalendarDate` 函数来创建日期对象，然后再使用 `Date.toString` 将日期对象转换为字符串。

## 参考资料：

- [Elm Documentation: Date](https://package.elm-lang.org/packages/elm/time/latest/Time-Date)
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)