---
title:                "获取当前日期"
html_title:           "Elm: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么是当前日期？为什么程序员要获取它？

当前日期是指当天的日期，通常以年、月、日的格式表示。程序员通常需要获取当前日期，以便在程序中进行日期相关的计算和操作，例如生成特定格式的日志或文件命名。

## 如何获取当前日期？

在 Elm 中，可以通过内置的 Date 模块来获取当前日期。以下是一个示例代码和输出：

```Elm
-- 导入 Date 模块
import Date exposing (Date, Day, fromTime)

-- 获取当前日期
currentDate : Date
currentDate = fromTime Date.now

-- 打印当前年份
currentYear : String
currentYear = toString currentDate.year

-- 输出结果
currentYear = "2021"
```

## 深入了解

- 历史背景：获取当前日期已经成为几乎所有编程语言的标准功能，以满足程序员的常见需求。
- 其他方法：除了使用内置的 Date 模块之外，还可以通过第三方库来获取当前日期，如 elm-time 或 terezka/elm-date-extra。
- 实现细节：Date 模块内部实际上使用的时区是 UTC，而不是当前所处的本地时区。如果需要使用本地时区，可以通过转换函数来实现。

## 查看更多

- [Elm Date 模块文档](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [elm-time 库](https://package.elm-lang.org/packages/ryannhg/elm-time/latest)
- [terezka/elm-date-extra 库](https://package.elm-lang.org/packages/terezka/elm-date-extra/latest/)