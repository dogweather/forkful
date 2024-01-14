---
title:    "Elm: 比较两个日期"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么
在现代生活中，我们经常需要比较两个日期。可能是为了确定哪个日期更旧，或者计算两个日期之间的时间间隔。使用Elm编程语言可以轻松地比较日期，并且它具有强大的类型安全性和纯函数的特性，使得代码更加可靠和易于维护。

## 如何使用
比较两个日期的最简单方法是使用```Date.compare```函数。下面是一个示例代码，展示如何使用它来比较两个日期，并根据结果输出相应的信息。

```Elm
firstDate = Date.fromYYMMDD 2021 10 10
secondDate = Date.fromYYMMDD 2021 10 15

case Date.compare firstDate secondDate of
    LT -> "第一天早于第二天"
    GT -> "第一天晚于第二天"
    EQ -> "两个日期相同"
```

输出结果为：```"第一天早于第二天"```，因为第一天比第二天早5天。

## 深入讨论
如果你需要更详细的比较结果，可以使用```Date.compareWith```函数。该函数返回一个```ComparisonResult```类型的结果，包含了比较结果的详细信息。下面是一个例子：

```Elm
firstDate = Date.fromYYMMDD 2021 10 10
secondDate = Date.fromYYMMDD 2021 10 15

case Date.compareWith firstDate secondDate of
    Date.Equal -> "两个日期相同"
    Date.LessThan -> "第一天早于第二天"
    Date.GreaterThan -> "第一天晚于第二天"
```

除了这些基本函数外，Elm社区还开发了许多日期比较相关的包，例如```justinmimbs/date-extra```包提供了更多的日期比较函数，而```elm-chronology```包则提供了更复杂的日期解析和操作功能。你可以根据自己的需求选择使用。

## 查看更多
如果你想了解关于Elm日期比较的更多信息，可以查看下面的链接：
- [Elm官方文档关于Date的介绍](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm时间和日期操作教程](https://guide.elm-lang.org/effects/time.html)
- [justinmimbs/date-extra：更多日期比较函数](https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/)
- [elm-chronology：复杂的日期操作包](https://package.elm-lang.org/packages/the-sett/elm-chronology/latest/)