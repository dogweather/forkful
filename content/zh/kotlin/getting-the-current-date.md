---
title:                "Kotlin: 获取当前日期"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么要获取当前日期

在当今的数字时代，时间和日期的重要性越来越明显。无论是在应用程序开发中还是在日常生活中，获取当前日期是非常常见的需求。通过获取当前日期，我们可以正确地跟踪时间，记录事件，以及进行相关的数据分析。

## 如何获取当前日期

在Kotlin中，我们可以使用`java.util`库中的`Date()`来获取当前日期。下面是一个简单的示例代码，展示如何使用`Date()`获取当前日期并将其打印出来：

```Kotlin
import java.util.*

fun main() {
    val currentDate = Date()
    println(currentDate) // 输出：Mon Jun 28 18:13:51 CST 2021
}
```

我们还可以使用`SimpleDateFormat`来格式化当前日期。下面是一个示例代码，展示如何将日期格式化为指定的格式：

```Kotlin
import java.util.*
import java.text.SimpleDateFormat

fun main() {
    val currentDate = Date()
    val dateFormat = SimpleDateFormat("yyyy-MM-dd")
    val formattedDate = dateFormat.format(currentDate)
    println(formattedDate) // 输出：2021-06-28
}
```

## 深入了解获取当前日期

在Kotlin中，我们还可以使用其他类来获取当前日期，例如`LocalDateTime`、`Calendar`等。每种方法都有其自己独特的优缺点，可以根据自己的需求选择适合的方法。此外，我们还可以通过修改时区设置来获取不同时区的当前日期。

## 查看更多资源

- [Kotlin官方文档-Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/index.html)
- [Java.util Date类介绍](https://www.runoob.com/manual/jdk1.6/java/util/Date.html)
- [Kotlin中日期时间的使用](https://www.jianshu.com/p/9421d0143b76)