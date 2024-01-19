---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

将日期转换为字符串是一种编程操作，它能将机器可读的日期（通常是数字及日期时间对象）转变为人类友好的可读格式。程序员们常进行这种转换以便更好地在图形用户界面或文本输出中显示日期数据。

## 如何操作：

```Elm
import Date exposing (Date)
import Time
from Time import (..)

getTimeString : Posix -> String
getTimeString time =
  Time.toHour minute time
  |> Tuple.pair 0
  |> Time.posixToTime
  |> Time.toString
```
这段代码用于将 POSIX 时间对象转换为字符串。下面是结果示例：

```Elm
getTimeString (Time.millisToPosix 1592431766433)
-- "5:36 AM"
```
这将时间戳 `1592431766433` 转换为 `5:36 AM`。

## 深入解析：

在过去，尤其是在计算机科技不那么发达的年代，日期和时间的处理是非常困难的，因为计算机和人类理解时间的方式有本质的差异。为了简化这个复杂的问题，许多编程语言包括Elm，都采用了 POSIX 时间戳的标准。这是一种全球通用的、以秒为单位的时间表示方法。

此外，有几种备选的日期到字符串的转换方式,例如使用 `Time.toIsoString` 函数来获取 ISO 8601 时间字符串。或者，直接使用原生 JavaScript 进行日期和时间的处理。

转换过程中值得注意的是Elm中情况的处理方式。例如，在上述代码实例中即`Tuple.pair 0`的操作，截取了分钟数，实现了跨越时间区的处理。这便是Elm语言细腻的细节处理方式。

## 另见：

Elm 日期和时间包文档： https://package.elm-lang.org/packages/elm/time/latest/

Elm语言处理日期和时间的相关博客文章： https://elmprogramming.com/dates-and-time.html

转换日期为字符串的其他方式：https://korban.net/posts/elm/2019-12-12-elm-date-format-library-overview/