---
title:                "获取当前日期"
html_title:           "Kotlin: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是一个常见的编程需求，它可以帮助开发者跟踪时间相关的任务，比如记录日志或者执行定时任务。

## 如何

在 Kotlin 中，获取当前日期可以通过使用 `LocalDate` 类来实现。首先，我们需要导入 `java.time.LocalDate` 包，然后在代码中创建一个 `LocalDate` 实例：

```Kotlin
import java.time.LocalDate

val currentDate = LocalDate.now()
```

我们也可以通过提供特定的日期 `year`, `month`, `dayOfMonth` 参数来创建一个自定义的 `LocalDate` 实例：

```Kotlin
val customDate = LocalDate.of(2021, 10, 31)
```

要格式化日期并以特定的格式输出，我们可以使用 `format` 方法：

```Kotlin
val formattedDate = currentDate.format(DateTimeFormatter.ISO_DATE)
println(formattedDate)
```

输出为 `2021-11-06`。

## 深入了解

除了使用 `LocalDate` 类，Kotlin 还提供了 `LocalDateTime` 类来获取当前日期和时间的详细信息。此外，还有 `ZonedDateTime` 类来转换日期和时间为特定的时区。

如果需要处理不同的时区或者日期和时间的计算，可以使用 `java.time` 包中提供的各种方法和类来实现。更多相关内容，可以查看 [Java 日期时间 API 文档](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)。

## See Also

- [Java 日期时间 API 文档](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)
- [官方 Kotlin 文档](https://kotlinlang.org/docs/home.html)