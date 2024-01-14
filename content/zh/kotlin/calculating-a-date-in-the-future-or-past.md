---
title:                "Kotlin: 计算未来或过去的日期"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算过去或未来的日期可能是在编程中经常遇到的问题。这可能是因为需要显示某个事件的开始日期，或者需要计算未来一周的日期。不管是什么原因，计算日期的能力对于程序员来说都是一个很有用的技能。

## 如何

```Kotlin
fun calculateDate(days: Int): String {
    // 获取当前日期
    val currentDate = LocalDate.now()
    // 使用plusDays方法计算未来或过去的日期
    val futureOrPastDate = currentDate.plusDays(days)
    // 格式化日期为"年-月-日"的形式
    val formattedDate = futureOrPastDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    // 返回最终的日期字符串
    return formattedDate
}

// 调用函数并打印结果
println("未来两天的日期为：" + calculateDate(2))
println("过去一周的日期为：" + calculateDate(-7))
```

## 深入探讨

在Kotlin中，我们可以使用Java 8中的日期和时间API来帮助我们计算日期。首先，我们使用`LocalDate.now()`来获取当前日期。然后，使用`plusDays()`方法来计算未来或过去的日期，我们可以通过传入正数来获得未来的日期，或者传入负数来获得过去的日期。最后，使用`DateTimeFormatter`来格式化日期为我们想要的形式。

## 参考链接

- [Kotlin Date and Time API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Java 8日期和时间API教程](https://www.baeldung.com/java-8-date-time-intro)