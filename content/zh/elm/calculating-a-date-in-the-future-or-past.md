---
title:                "计算未来或过去的日期"
html_title:           "Elm: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么&为什么?

计算未来或过去的日期是一种计算距离当前日期特定天数的方法。程序员通常会在策划事件，计划任务，或者跟踪时间这样的功能中使用到。

## 如何实现:

在Elm中，我们可以使用`Date.addDays`方法来计算未来或过去的日期。以下是示例代码：

```Elm
import Date exposing (..)

main = 
    let 
        today = fromTime <| Time.posix 1590968761000
        futureDate = Date.add Days 10 today
    in
    text <| toString futureDate
```

以上代码的输出结果应为：“2020-06-10”。这表示从1590968761000（即2020-5-31）开始后的10天是2020-6-10。

## 深入剖析:

历史背景: Elm语言中的`Date`模块提供了多种处理日期的功能，是在0.17版本后引入的，并在后续的版本中不断优化。

替代方案: 我们还可以使用`Date.diff`方法来计算两个日期之间的差距。

实现细节: `Date.addDays`方法将时间参数转换为毫秒数，然后加上相应天数的毫秒数，最后再转回日期格式。

## 另请参见: 

- Elm日期模块文档: http://package.elm-lang.org/packages/elm/time/latest/Date
- Elm时间处理教程: https://korban.net/posts/elm/posts/elm-date-and-time-basics/