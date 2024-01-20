---
title:                "计算未来或过去的日期"
html_title:           "Kotlin: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么与为什么?

未来或过去的日期计算是计算出从现在起指定天数之后或之前的日期。程序员经常需要做这个，例如用于计划未来的任务，或查找过去的日期。

## 如何：

Kotlin使用`java.time.LocalDate`类，通过调用`plusDays`或`minusDays`函数，我们能够实现计算未来或过去的日期。

```kotlin
import java.time.LocalDate

fun main() {
    val presentDate = LocalDate.now()
    println("今天的日期是 : $presentDate")

    val futureDate = presentDate.plusDays(10)
    println("10天后的日期是 : $futureDate")

    val pastDate = presentDate.minusDays(5)
    println("5天前的日期是 : $pastDate")
}
```
这段程序会输出类似：

```kotlin
今天的日期是 : 2022-03-25
10天后的日期是 : 2022-04-04
5天前的日期是 : 2022-03-20
```

## 深度解析 

1) 历史背景：
从JDK 8开始，Java开始使用新的日期、时间库`java.time`来替代原有的`java.util.Date`和`java.util.Calendar`。Kotlin是完全兼容Java的，因此对这些类的操作都是一致的。

2) 其他做法：
除此之外，使用`java.util.Calendar`也可以实现类似的功能，但对于新的代码，推荐用`java.time`。

3) 实现细节：
`plusDays`和`minusDays`函数会返回一个新的`LocalDate`实例，不会改变原有的实例。

## 另见

[Oracle Java文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#plusDays-long-): `java.time.LocalDate`的官方文档，包括`plusDays`和`minusDays`的详细描述。

[Kotlin官方文档](https://kotlinlang.org/docs/dates-and-times.html): Kotlin语言关于日期和时间操作的官方文档。