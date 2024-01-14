---
title:                "Elm: 比较两个日期"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么

比较日期在编程中经常会用到，因为它可以帮助我们判断哪个日期在前，哪个日期在后，从而方便我们进行事件的排序和处理。

## 如何进行比较

在Elm中，我们可以使用内置的Date模块来进行日期比较。首先，我们需要引入该模块并创建两个日期变量，然后使用Date.compare函数来比较它们。比较结果将返回一个Order类型的值，包括LT（小于）、EQ（等于）和GT（大于）三种可能性。下面是一个简单的示例代码和输出：

```Elm
import Date exposing (..)

date1 = Date.fromDate 2021 8 1
date2 = Date.fromDate 2021 7 1

comparison = Date.compare date1 date2

-- 输出：
-- GT
```

## 深入探讨

在Elm中，日期被表示为一个Record类型，其中包含year、month和day三个字段。因此，Date模块提供了一些辅助函数来方便我们创建和操作日期变量。例如，我们可以使用Date.fromTime函数从时间戳创建日期，使用Date.toTime函数将日期转换为时间戳，以及使用Date.monthsBetween函数来计算两个日期之间相差的月份数。除此之外，Elm社区还提供了一些有用的日期比较的第三方包，如elm-time和elm-calendar，可以帮助我们更方便地进行日期处理。

## 参考资料

- [Elm Date模块文档](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm 比较日期官方示例](https://elm-lang.org/examples/compare-dates)
- [elm-time包文档](https://package.elm-lang.org/packages/justinmimbs/time/latest/)
- [elm-calendar包文档](https://package.elm-lang.org/packages/arowM/elm-calendar/latest/)