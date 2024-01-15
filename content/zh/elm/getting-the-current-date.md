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

## 为什么？

有几种可靠的方法可以获取当前日期。无论是为管理工具记录时间，还是为了实现一个时钟， 获取当前日期对于Web开发和桌面应用程序都是必不可少的。在本文中，我将介绍如何使用 Elm 程序语言获取当前日期。

## 怎么做？

要获取当前日期，首先需要 import " Time" 模块。然后，使用 " Time.now " 函数来获取当前日期的时间戳。

```Elm
import Time

Time.now
```

这将输出一个 "Posix" 类型的时间戳，在 Elm 中表示为整数。这个时间戳可以进一步转换为其他格式，例如字符串或日期对象。

```Elm
import Time
import Date

let
    timestamp = Time.now
    date = Date.fromTime timestamp
    formattedDate = Date.format "%Y-%m-%d" date
in
    formattedDate
```

在这个例子中，我们使用了 " Date " 模块将时间戳转换为可读的日期格式，然后使用 " Date.format " 函数将其格式化为 "年-月-日" 的字符串。

## 深入了解

在 Elm 中，时间戳是以毫秒为单位的整数，从 1970 年 1 月 1 日 UTC 开始计算。这种格式被称为 "Posix" 时间，它是计算机中最常用的时间格式之一。通过使用 " Time " 模块，我们可以方便地将 " Posix " 时间戳转换为更友好的日期格式，并通过 " Date.format " 函数自定义日期的显示方式。

## 参考链接

- [Elm Documentation](https://elm-lang.org/docs)
- [Elm Time Module](https://package.elm-lang.org/packages/elm/time/latest/)
- [Date Formatting in Elm](http://package.elm-lang.org/packages/elm/time/latest/Date#format)
- [Posix Time Explanation](https://en.wikipedia.org/wiki/Unix_time)