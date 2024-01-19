---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么?
比较两个日期意味着你正在检查这两个日期哪一个在日历上排在前面。这对于程序员来说很重要，例如在排序日历事件，检查一个日期是否已过期等方面。

## 如何做

```Elm
import Time 

compareDates : Time.Zone -> Time.Posix -> Time.Posix -> Order
compareDates zone date1 date2 = 
    Time.toYear zone date1 |> compare (Time.toYear zone date2)

-- 接下来是样例
main = 
    let 
        zone = Time.here
        date1 = Time.millisSinceEpoch 1556698143000
        date2 = Time.millisSinceEpoch 1627804987000
    in 
    Html.text (toString (compareDates zone date1 date2))
-- 输出结果：LT，意味着第一个日期小于第二个日期。
```

## 深度探讨
Elm 对日期的处理最早来源于时间函数库(Time libraries) Time.Posix 和 Time.Zone，这两个库提供了一套完备的解决方案来处理日期和时间。尽管Elm也可以通过直接比较毫秒来确定日期的早晚，但使用Time库使得程序更加清晰且可读性更强。在实现上，Elm的比较函数实际上就是将日期转换成了从公元1年开始的年份，然后进行比较。

## 参考资料
你可以从以下资源获得更多关于比较两个日期的信息：
1. [Elm Time库文档](https://package.elm-lang.org/packages/elm/time/latest/)
2. [Elm discuss: Date Comparison](https://discourse.elm-lang.org/t/date-comparison/2649)
3. [Elm入门教程](https://tech.paiza.io/entry/2016/05/30/140814be) (日语，麦爪翻译)