---
title:                "计算过去或未来的日期"
html_title:           "Kotlin: 计算过去或未来的日期"
simple_title:         "计算过去或未来的日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算未来或过去的日期可能是一个很有用的工具，它可以帮助我们预测未来事件的发生日期，或者回顾过去事件的发生日期。这在许多情况下都是必需的，比如计划旅行、制定项目计划、纪念日等。使用 Kotlin 编程语言来实现日期计算功能，可以让我们更加方便地完成这些任务。

## 如何操作

首先，我们需要引入 `java.time.LocalDate` 包，它提供了日期的计算和操作功能。然后，我们可以使用 `plusDays()` 和 `minusDays()` 方法来分别对日期进行向后和向前的计算。下面是一个示例代码：

```Kotlin
import java.time.LocalDate

// 计算 10 天后的日期
val dateAfterTenDays = LocalDate.now().plusDays(10)
println("今天的日期是：${LocalDate.now()}")
println("十天后的日期是：$dateAfterTenDays")

// 计算 30 天前的日期
val dateBeforeThirtyDays = LocalDate.now().minusDays(30)
println("今天的日期是：${LocalDate.now()}")
println("三十天前的日期是：$dateBeforeThirtyDays")
```

运行后，我们可以得到如下输出：

```
今天的日期是：2021-01-01
十天后的日期是：2021-01-11

今天的日期是：2021-01-01
三十天前的日期是：2020-12-02
```

## 深入了解

除了使用 `plusDays()` 和 `minusDays()` 方法，我们还可以使用 `plusMonths()`、`minusMonths()`、`plusYears()`、`minusYears()` 方法来对日期进行月份和年份的计算。另外，我们也可以在 `LocalDate` 对象上直接调用 `plus()` 和 `minus()` 方法，传入 `java.time.Period` 对象来进行更加复杂的日期计算。

需要注意的是，在日期计算中，我们并不需要考虑闰年的问题，因为 `LocalDate` 会自动进行处理。

另外，Kotlin 也提供了 `java.time.LocalDateTime`、`java.time.LocalTime` 等类来处理日期和时间的计算，可以根据具体的需求选择使用。

## 相关阅读

如果你想了解更多关于使用 Kotlin 计算日期的方法，可以参考下面的链接：

1. Kotlin 文档： https://kotlinlang.org/docs/datetime.html
2. Java API 文档： https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html

## 查看相关

- Kotlin 官方网站： https://kotlinlang.org/
- Java 官方网站： https://www.java.com/
- Android 开发者网站： https://developer.android.com/