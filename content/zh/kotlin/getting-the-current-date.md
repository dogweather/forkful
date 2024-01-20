---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
获取当前日期是指编程返回当期读时间的过程。程序员这样做是因为各种操作，例如记录事务发生的实际时间，跟踪程序运行的时间，或只是显示当前日期给用户。

## 怎么做：
在 Kotlin 中，你可以这样获取当前日期：

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("当前日期是: $currentDate")
}
```
当运行此代码时，输出将会是：
```Kotlin
当前日期是: 2022-12-15 //根据当前日期的变化而变化
```

## 深度解析
Java 8 之前，你需要使用 `java.util.Date`，但这个类的可用性并不好。因此，在 Java 8 中，引入了新的日期时间 API，其中包括 `LocalDate` 类，你可以用它来获取当前日期。

Kotlin 允许 Java 中所有类型的代码，所以我们可以使用 Java 的 `LocalDate.now()` 来获取当前日期。你也可以通过使用 `java.util.Calendar.getInstance()` 得到更多详细的日期和时间信息。

请注意，如果你需要显示给用户，那么须根据用户的时间偏好或者时区偏好进行格式化日期。

## 查阅资料
1. [官方文档-LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [关于 Kotlin 的日期和时间的更多信息](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
3. [关于古老的 java.util.Date 的更多信息](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)