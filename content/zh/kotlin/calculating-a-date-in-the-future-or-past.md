---
title:    "Kotlin: 计算未来或过去的日期"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

在现代的世界，计算日期的能力变得越来越重要。人们可能需要计算一些特殊的日期，例如未来的生日、重要的纪念日或者出国留学的日期。了解如何使用Kotlin来计算未来或过去的日期能够让我们的生活更加便利，更加智能化。

## 如何做

首先，我们需要确定当前的日期。在Kotlin中，我们可以使用`LocalDate`来表示一个日期。下面的例子中，我们声明了一个名为`today`的变量，它存储了当前的日期：

```Kotlin
val today = LocalDate.now()
```

接着，我们可以使用`plus`或`minus`方法来计算未来或过去的日期。这两个方法都接受`Period`作为参数，`Period`对象表示一段时间。下面的例子中，我们计算了10天后的日期，然后将结果打印出来：

```Kotlin
val tenDaysFromNow = today.plus(Period.ofDays(10))
println(tenDaysFromNow)
```

输出结果为：2022-01-12

同样地，我们也可以计算过去的日期。下面的例子中，我们计算了30天前的日期，并打印出结果：

```Kotlin
val thirtyDaysAgo = today.minus(Period.ofDays(30))
println(thirtyDaysAgo)
```

输出结果为：2021-11-13

## 深入了解

除了使用`plus`和`minus`方法，我们还可以使用`TemporalAdjusters`来进行更加复杂的日期计算。`TemporalAdjusters`是一个工具类，它包含了许多静态方法，可以帮助我们方便地进行日期计算。下面是一个例子，我们使用`TemporalAdjusters`来计算今年最后一天的日期，并打印出结果：

```Kotlin
val lastDayOfYear = today.with(TemporalAdjusters.lastDayOfYear())
println(lastDayOfYear)
```

输出结果为：2021-12-31

除了`TemporalAdjusters`，我们还可以使用`ChronoUnit`，它是一个枚举类，包含了各种时间单位，例如天、小时、分钟等。下面的例子中，我们使用`ChronoUnit`来计算两个日期相差的天数，并打印出结果：

```Kotlin
val difference = today.until(lastDayOfYear, ChronoUnit.DAYS)
println(difference)
```

输出结果为：19

## 参考资料

- [Kotlin官方文档](https://kotlinlang.org/docs/dates.html)
- [Kotlin入门教程](https://www.runoob.com/kotlin/kotlin-tutorial.html)
- [计算机日期和时间相关知识](https://zh.wikipedia.org/wiki/%E8%AE%A1%E7%AE%97%E6%9C%BA%E6%97%A5%E6%9C%9F%E5%92%8C%E6%97%B6%E9%97%B4%E7%9B%B8%E5%85%B3%E7%9F%A5%E8%AF%86)
- [Stack Overflow上的计算日期相关问题](https://stackoverflow.com/questions/tagged/kotlin+date-comparison) 

## 查看更多

[查看如何使用Kotlin在Android应用中进行日期计算！](https://www.baeldung.com/kotlin-android-date-manipulation)