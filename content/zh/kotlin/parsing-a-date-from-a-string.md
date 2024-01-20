---
title:                "从字符串解析日期"
date:                  2024-01-20T15:37:15.535832-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - 什么 & 为什么?
解析日期就是把文本格式的日期转换成计算机可以理解的格式。程序员这么做的原因包括验证数据格式、执行日期运算或者存储和比较日期。

## How to - 怎么做
在Kotlin中，我们使用`java.time`库来解析日期。这是个例子：

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDate {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    return LocalDate.parse(dateString, formatter)
}

fun main() {
    val date = parseDateFromString("2023-03-31")
    println(date) // 打印解析出来的日期
}
```

运行后的输出：

```
2023-03-31
```

## Deep Dive - 深入探讨
Kotlin作为一门运行在JVM上的语言，可利用Java提供的强大的日期与时间API。在Java 8之前，大家常用`SimpleDateFormat`，但是它不是线程安全的，易出错。`java.time`包是Java 8中引入的，解决了这些问题，并且用起来更简单直观。

除了`LocalDate`外，如果你需要包含时间信息，可以用`LocalDateTime`。还有很多其他类和方法，比如`ZonedDateTime`可以处理时区。

实施数据解析时，记得考虑异常处理。如果输入的文本格式不正确，`DateTimeParseException`会被抛出。在实际应用中，我们应该捕获这个异常，并给用户合适的反馈。

## See Also - 参考链接
- Oracle 官方文档：[Date Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Kotlin语言官方文档：[Kotlin documentation](https://kotlinlang.org/docs/reference/)
- 关于Java 8时间日期API的进一步解读：[Understanding Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)