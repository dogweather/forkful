---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:36.451413-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u6587\
  \u672C\u8F6C\u6362\u6210\u65E5\u671F\u5BF9\u8C61\u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\
  \u4E8E\u9700\u8981\u4E0E\u7528\u6237\u8F93\u5165\u6216\u6765\u81EA\u5916\u90E8\u6570\
  \u636E\u96C6\u7684\u65E5\u671F\u4EA4\u4E92\u7684\u5E94\u7528\u7A0B\u5E8F\u800C\u8A00\
  \uFF0C\u662F\u57FA\u7840\u6027\u7684\uFF0C\u5141\u8BB8\u6839\u636E\u9700\u8981\u8F7B\
  \u677E\u5730\u8FDB\u884C\u64CD\u4F5C\u548C\u683C\u5F0F\u5316\u3002"
lastmod: 2024-02-19 22:05:06.761274
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\u6587\
  \u672C\u8F6C\u6362\u6210\u65E5\u671F\u5BF9\u8C61\u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\
  \u4E8E\u9700\u8981\u4E0E\u7528\u6237\u8F93\u5165\u6216\u6765\u81EA\u5916\u90E8\u6570\
  \u636E\u96C6\u7684\u65E5\u671F\u4EA4\u4E92\u7684\u5E94\u7528\u7A0B\u5E8F\u800C\u8A00\
  \uFF0C\u662F\u57FA\u7840\u6027\u7684\uFF0C\u5141\u8BB8\u6839\u636E\u9700\u8981\u8F7B\
  \u677E\u5730\u8FDB\u884C\u64CD\u4F5C\u548C\u683C\u5F0F\u5316\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
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
