---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么&为什么?

字符串解析日期是一种将日期从字符串格式转换为计算机能理解的日期格式的过程。程序员这样做以便能正确读取、比较和操作日期数据。

## 如何操作:

我们可以用Elm的内建函数来进行解析。

```Elm
import Time

parseDate : String -> Result String Time.Posix
parseDate dateStr =
    Time.fromString dateStr

parseResult =
    case parseDate "2021-09-01T13:30:00Z" of
        Ok date ->
            "Success: " ++ (Time.posixToMillis date |> String.fromInt)

        Err _ ->
            "Failed to parse date."
```

这会输出:

```Elm
"Success: 1630506600000"
```

## 深入洞察

在早期的计算机应用中，串行格式字串是常见的日期格式。这是由于存储和带宽限制,以及可读性.然而, 由于串行的组织方式,并不是所有的编程语言都能很好的解析.

Elm在1.0.0版本引入了Time模块以更好地支持日期和时间处理。你也可以用其他库如 "elm-date-extra" 扩展日期处理功能。

在实现上,Elm将日期解析为`Posix`类型,其中包含从1970年1月1日（UTC）以来的毫秒数。这是一种标准的日期储存格式，使得日期比较和计算更有效率。

## 另见

1. Elm的官方时间模块文档:[Time](https://package.elm-lang.org/packages/elm/time/latest/Time)


3. 其他日期解析及操作库: [elm-date-extra](https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest/)