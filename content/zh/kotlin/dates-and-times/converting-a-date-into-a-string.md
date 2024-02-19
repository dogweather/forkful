---
aliases:
- /zh/kotlin/converting-a-date-into-a-string/
date: 2024-01-20 17:36:46.375446-07:00
description: "\u628A\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\
  \u65E5\u671F\u683C\u5F0F\u6539\u6210\u6587\u672C\u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u7684\u539F\u56E0\u662F\u4E3A\u4E86\u66F4\u5BB9\u6613\u5730\u663E\
  \u793A\u548C\u50A8\u5B58\u65E5\u671F\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.105311
model: gpt-4-1106-preview
summary: "\u628A\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\
  \u65E5\u671F\u683C\u5F0F\u6539\u6210\u6587\u672C\u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u7684\u539F\u56E0\u662F\u4E3A\u4E86\u66F4\u5BB9\u6613\u5730\u663E\
  \u793A\u548C\u50A8\u5B58\u65E5\u671F\u6570\u636E\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
把日期转换成字符串就是将日期格式改成文本形式。程序员这么做的原因是为了更容易地显示和储存日期数据。

## How to: (如何操作：)
```Kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val currentDate = Date()
    val dateFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault())
    val dateString = dateFormat.format(currentDate)
    println(dateString)  // 输出例如 "2023-04-03 15:21:47"
}
```

## Deep Dive (深入探索)
在Kotlin中，转换日期到字符串不单只是为了方便存储和展示，它还关涉到数据的国际化和本地化。历史上，Java的 `SimpleDateFormat` 类是处理日期格式化的主要方法。虽然Kotlin是基于Java平台，但是它提供了更丰富的API和改进的方法比如使用 `java.time` （Java 8+）去处理日期和时间。其他的替代方案包括使用第三方图书馆比如 Joda-Time。在执行数据转换时，我们需考虑时区(`TimeZone`)和区域(`Locale`)的影响。

## See Also (另请参阅)
- [Kotlin官方文档](https://kotlinlang.org/docs/home.html)
- [SimpleDateFormat文档](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [LocalDate和LocalDateTime文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
