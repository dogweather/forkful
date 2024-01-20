---
title:                "获取当前日期"
date:                  2024-01-20T15:15:14.308187-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
获取当前日期意味着通过编程获取系统当前的日期值。程序员这样做是为了记录事件发生的时间，或者是为了功能需要，比如创建日历应用。

## How to: 怎么做：
```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("今天的日期是：$currentDate")
}

// 输出样例：
// 今天的日期是：2023-04-05
```

## Deep Dive 深入探讨
在Java 8 发布之前，我们常用 `java.util.Date` 或 `Calendar` 类来获取当前日期。但是，这些类存在设计问题，比如线程安全和时区处理问题。Java 8 引入了 `java.time` 包，提供了新的日期时间API，`LocalDate` 是其中的关键类之一，专门用来表示不包含时间的日期。此类提供了静态方法 `now()` 来获取当前日期。在Kotlin中，这一操作非常简单直观。此外，你还可以使用 `java.time.ZonedDateTime` 来获取带时区的完整日期和时间，或者用 `java.time.Clock` 对象来指定时钟，以便于测试或者其他特殊需求。

## See Also 查看更多
- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)
- [java.time.LocalDate官方文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [关于Java时间日期API的改进讨论](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)