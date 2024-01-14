---
title:    "Kotlin: 将日期转换为字符串"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## 为什么

日期和时间在编程中经常被使用，因为它们是程序的基本组成部分。将日期表示为字符串，可以帮助我们更方便地在程序中使用它们，比如存储到数据库中或者展示给用户。在这篇文章中，我们会学习如何将日期转换成字符串，让我们的程序更加灵活和智能。

## 如何做

日期转换成字符串在Kotlin中非常简单。我们只需要使用 `String.format()` 方法，并指定日期的格式即可。下面是一个例子：

```Kotlin 
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
  // 获取当前日期
  val date = LocalDate.now()
  // 定义日期格式
  val pattern = "MM/dd/yyyy"
  // 使用String.format()方法进行转换
  val stringDate = String.format(pattern, date)
  // 输出字符串格式的日期
  println(stringDate)
}
```

上面的代码会输出当前日期的格式化字符串，如：04/23/2021。我们可以通过指定不同的日期格式，来输出不同的字符串日期。例如，我们可以将 `pattern` 改为 "dd-MM-yyyy"，即输出 23-04-2021。下面是Kotlin官方文档中列举的一些常用的日期和时间格式：

- `MM/dd/yyyy`：日期格式，例如 04/23/2021
- `dd-MM-yyyy`：日期格式，例如 23-04-2021
- `yyyy-MM-dd`：日期格式，例如 2021-04-23
- `yyyy/MM/dd`：日期格式，例如 2021/04/23
- `HH:mm:ss`：24小时制时间，例如 14:36:45
- `h:mm a`：12小时制时间，例如 2:36 PM
- `dd-MMM-yyyy`：日期格式，例如 23-Apr-2021
- `E, MMM dd yyyy`：日期格式，例如 Fri, Apr 23 2021

## 深入了解

Kotlin中，日期是通过 `java.time.LocalDate` 类来表示的。我们可以使用该类提供的方法来操作日期，比如获取当前日期、增加或减少天数等。另外，Kotlin还提供了 `java.time.format.DateTimeFormatter` 类，用于定义日期的格式。该类提供了各种各样的格式，可以满足我们的需求。你也可以通过 `DateTimeFormatter.ISO_DATE` 来获取一个默认的日期格式。

总的来说，将日期转换成字符串是一项有用的任务，可以帮助我们更灵活地使用日期和时间，让程序更加智能化。在实际开发中，我们也可以根据自己的需求自定义日期格式，并将其转换成字符串输出。

## 更多阅读

如果你想进一步了解Kotlin中的日期和时间操作，可以参考下面的文档和教程：

- [Kotlin官方文档：日期和时间](https://kotlinlang.org/docs/tutorials/datetime/datetime.html)
- [Kotlin官方文档：字符串格式化](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- [Tutorialspoint教程：Kotlin日期和时间操作](https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm)
- [GeeksforGeeks教程：字符串格式化](https://www.geeksforgeeks.org/kotlin-strings-formatted-strings/)

# 参考链接

- [Kotlin官方文档](https://kotlinlang.org/docs/home.html)
- [Tutorialspoint教程](https://www.tutorialspoint.com/index.htm)
- [GeeksforGeeks教程](https://www.geeksforgeeks.org/)

# 参见
- [Java虚拟机调试指南](https://github.com/ABBYY-MIPT/jvm-web-debugger/wiki/Overview)