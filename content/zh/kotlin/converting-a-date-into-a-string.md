---
title:                "Kotlin: 将日期转换为字符串"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

日期是程序中常见的数据类型，经常需要将其转换为字符串以便在界面上显示或存储到数据库中。因此，学习如何将日期转换为字符串是Kotlin编程中必不可少的技能。

## 如何做

首先，我们需要创建一个日期对象。假设我们要将今天的日期转换为字符串，可以使用```LocalDate.now()```方法来获取当前日期。接下来，我们可以使用```format()```方法将日期格式化为我们想要的字符串形式。例如，下面的代码将把日期格式化为"yyyy-MM-dd"的形式：

```Kotlin
val today = LocalDate.now()
val stringDate = today.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
println(stringDate)
```

输出为：2021-01-01

我们也可以将日期格式化为中文的形式，例如"yyyy年M月d日"。代码如下：

```Kotlin
val stringDate = today.format(DateTimeFormatter.ofPattern("yyyy年M月d日"))
println(stringDate)
```

输出为：2021年1月1日

除了年月日，我们还可以将日期转换为包含时分秒的字符串。例如，下面的代码将把日期格式化为"yyyy-MM-dd HH:mm:ss"的形式：

```Kotlin
val stringDate = today.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
println(stringDate)
```

输出为：2021-01-01 00:00:00

需要注意的是，不同的字母代表不同的日期格式，具体可参考[文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/format.html)。同时，我们也可以自己定义日期格式中各个部分的顺序和分隔符，例如"MM/dd/yy"表示月/日/年的形式。

## 深入了解

在Kotlin中，日期和时间的处理主要依赖于Java的[Date-Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)。我们可以通过```java.time.format.DateTimeFormatter```类提供的方法来格式化日期。同时，我们也可以使用[SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)类进行日期格式化，不过在多线程环境下不推荐使用。

总的来说，日期和字符串的相互转换在Kotlin中十分便捷。通过上面的方法，我们可以轻松地将日期转换为不同的字符串形式，使其能够适应各种需求。

## 参考链接

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)
- [Java Date-Time API文档](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)