---
title:                "计算未来或过去的日期。"
html_title:           "Kotlin: 计算未来或过去的日期。"
simple_title:         "计算未来或过去的日期。"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 是什么 & 为什么？
在编程中，经常会遇到需要计算未来或过去某个日期的情况。比如，某个活动需要在下个月的第二个星期六举行，或者需要在十天后提醒用户某件事。为了方便处理这种日期相关的逻辑，程序员需要使用代码来计算日期。

# 怎么做：
使用Kotlin内置的 `LocalDate` 类，可以很容易地进行日期计算。首先，我们需要导入 `java.time` 包，然后就可以使用 `now()` 方法获取当前日期，或者使用 `of()` 方法指定特定的日期。接下来，使用 `plus()` 方法来计算未来的日期，或者使用 `minus()` 方法来计算过去的日期。最后，我们可以使用 `format()` 方法来将日期格式化为我们需要的形式。

```kotlin
import java.time.*

fun main() {
    // 获取当前日期
    val today = LocalDate.now()

    // 计算未来日期，返回结果为2021-10-16
    val futureDate = today.plus(Period.ofMonths(1)).with(TemporalAdjusters.next(DayOfWeek.SATURDAY))

    // 计算过去日期，返回结果为2021-09-25
    val pastDate = today.minus(10L, ChronoUnit.DAYS)

    // 格式化日期为yyyy/MM/dd形式
    println(futureDate.format(DateTimeFormatter.ofPattern("yyyy/MM/dd")))
}
```

# 深入了解：
- 历史背景：在早期的计算机编程中，日期计算并不是那么容易，需要编写复杂的代码来处理。但随着现代编程语言的发展，日期计算变得更加简单，比如Kotlin就支持直接操作日期对象。
- 其他替代方案：除了使用Kotlin内置的日期类，也可以使用第三方库来处理日期计算。比如 `Joda-Time` 和 `DateUtils` 等。
- 实现细节：在 `LocalDate` 类中，日期是以 `Year-Month-Day` 的格式存储的，并且支持闰年的计算。使用 `plus()` 和 `minus()` 方法时，可以传入 `Period` 对象来指定想要增加或减少的时间单位，也可以使用 `TemporalAdjusters` 类来指定具体的日期，比如下一个周六。
# 相关资源：
- Kotlin官方文档：https://kotlinlang.org/docs/datetime.html
- Joda-Time库：https://www.joda.org/joda-time/
- DateUtils库：https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/time/DateUtils.html