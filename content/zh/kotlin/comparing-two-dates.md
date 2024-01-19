---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么&为什么？

日期比较是计算两个日期之间的差异。开发人员这样做是为了进行各种操作，如计算天数，验证输入的日期是否在规定的范围内等。

## 如何操作：

在Kotlin中，我们使用 `compareTo()` 函数来比较两个日期。如果第一个日期早于第二个日期，则返回 -1；如果两个日期相等，返回 0 ; 如果第一个日期晚于第二个日期，返回 1。

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2021, 1, 1)
    val date2 = LocalDate.of(2021, 1, 2)

    when {
        date1.compareTo(date2) < 0 -> println("date1 is before date2")
        date1.compareTo(date2) == 0 -> println("date1 is equal to date2")
        date1.compareTo(date2) > 0 -> println("date1 is after date2")
    }
}
```

上述代码的输出将是：

```
date1 is before date2
```

## 深入探究：

历史上，日期比较一直具有挑战性，因为需要考虑日历的不同形式和全球时间的变化。Kotlin提供的 `compareTo()` 方法便捷且快速，且考虑了所有这些因素，使得日期比较更为简单。

一种可替代的方法是使用 `isBefore()` 或 `isAfter()` 函数。这两个函数分别用于检查一个日期是否在指定日期前或后。

实现细节中，日期比较取决于许多因素，包括时区和日历制度。例如，公历与农历之间的日期比较需要特殊处理。

## 参见：

- [日期和时间的Kotlin官方文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js.date/-date/index.html)
- [Kotlin日期时间API的Java文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [关于Kotlin日期比较的StackOverflow讨论](https://stackoverflow.com/questions/44680602/comparing-dates-java-8-date-time-api-and-kotlin)