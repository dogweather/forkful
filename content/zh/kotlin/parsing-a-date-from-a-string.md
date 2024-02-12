---
title:                "从字符串解析日期"
aliases:
- zh/kotlin/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:36.451413-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 是什么 & 为什么？
从字符串解析日期涉及将文本转换成日期对象。这个操作对于需要与用户输入或来自外部数据集的日期交互的应用程序而言，是基础性的，允许根据需要轻松地进行操作和格式化。

## 如何操作：
Kotlin通过`java.time`包支持日期解析，该包在Java 8中引入。这里有一个使用`LocalDateTime`和特定模式的简单方法：

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // 输出：2023-04-01T12:00
}
```

为了更大的灵活性，或者处理来自诸如API之类的外部来源的日期，你可能会使用第三方库，如Joda-Time（尽管现在随着`java.time`的强大，它变得不那么常见）。然而，对于大多数Kotlin应用程序，坚持使用JDK提供的现代方法是首选。

要在Kotlin中不使用第三方库解析日期，如果你使用的是Java 8之前的版本或Android API级别不支持`java.time`的情况，你还可以使用`SimpleDateFormat`类：

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // 输出将基于你的时区有所不同，例如，Sat Apr 01 12:00:00 GMT 2023
}
```

记住，如果使用`SimpleDateFormat`，始终设置时区以避免解析日期时出现意外的偏移。
