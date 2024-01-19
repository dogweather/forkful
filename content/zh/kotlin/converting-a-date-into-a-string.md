---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
日期转换为字符串是一种常见的编程任务，它将Date对象转换为易读的文本形式。这样做能更方便地在UI中展示日期，或者存储和传输数据。

## 怎么做:
在Kotlin中使用java.text.SimpleDateFormat类可以很容易地实现日期到字符串的转换。示例代码如下：

```Kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date()
    val format = SimpleDateFormat("yyyy.MM.dd, 'at' HH:mm:ss z")
    println("Current date/time: ${format.format(date)}")
}
```

运行上述代码，输出将显示当前日期和时间：

```
Current date/time: 2022.10.10, at 16:40:20 CST
```

## 深入剖析:

- 历史背景: SimpleDateFormat是Java从1.1版本开始就有的类，可以分别设置日期和时间的格式化样式。Kotlin可以调用Java的这个类来实现日期和时间的字符串化。

- 替代方法: 你还可以使用Java 8引入的java.time API来完成这个任务。

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy.MM.dd, 'at' HH:mm:ss z")
    println("Current date/time: ${current.format(formatter)}")
}
```

- 实现细节: 在字符串化日期时，可以用不同的符号来表示年、月、日、小时、分钟和秒。例如,"yyyy"是四位年份，"MM"是两位月份，"dd"是两位日期，"HH"是24小时制的小时，"mm"是分钟，"ss"是秒，"z"是时区。符号的具体含义可以参考Java文档。

## 另请参阅:
- Java SimpleDateFormat类官方文档 [链接](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- Kotlin官方文档关于调用Java类的指南 [链接](https://kotlinlang.org/docs/java-interop.html)
- Java 8 java.time包官方文档 [链接](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)