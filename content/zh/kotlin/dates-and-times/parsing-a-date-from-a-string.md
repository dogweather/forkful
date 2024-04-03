---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:36.451413-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Kotlin\u901A\u8FC7`java.time`\u5305\u652F\
  \u6301\u65E5\u671F\u89E3\u6790\uFF0C\u8BE5\u5305\u5728Java 8\u4E2D\u5F15\u5165\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528`LocalDateTime`\u548C\u7279\u5B9A\u6A21\
  \u5F0F\u7684\u7B80\u5355\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.730154-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u901A\u8FC7`java.time`\u5305\u652F\u6301\u65E5\u671F\u89E3\u6790\uFF0C\
  \u8BE5\u5305\u5728Java 8\u4E2D\u5F15\u5165\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\
  \u7528`LocalDateTime`\u548C\u7279\u5B9A\u6A21\u5F0F\u7684\u7B80\u5355\u65B9\u6CD5\
  \uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
