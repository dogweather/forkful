---
title:    "Kotlin: 获取当前日期"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么
获取当前日期是每个程序员都会遇到的任务。无论是在日常的工作中还是做项目开发，我们都会需要使用到日期。因此，了解如何获取当前日期是非常有用的。

## 如何
在Kotlin中，获取当前日期最简单的方法是使用``java.time.LocalDate``类。以下是一个例子：
```
val currentDate = LocalDate.now()
println(currentDate)
```
输出应为：2021-09-11

我们也可以根据需求获取特定格式的日期，例如：
```
val currentDate = LocalDate.now()
println("今天是${currentDate.year}年${currentDate.monthValue}月${currentDate.dayOfMonth}日")
```
输出应为：今天是2021年9月11日

## 深入了解
在Kotlin中，``LocalDate``类是专门用来表示日期的。它使用了ISO-8601标准来表示日期格式，即``yyyy-MM-dd``。我们还可以通过使用``LocalDate.parse()``方法来将一个字符串转换为日期对象。例如：
```
val date = LocalDate.parse("2021-09-11")
println(date)
```
输出应为：2021-09-11

除了``LocalDate``类，我们还可以使用``LocalDateTime``类来表示日期和时间，以及``LocalTime``类来表示时间。这些类都有各自的方法来获取当前的日期、时间或日期时间。

# 参考链接
- [Kotlin官方文档：日期与时间](https://kotlinlang.org/docs/datetime.html)
- [使用Kotlin获取日期与时间](https://www.baeldung.com/kotlin/dates)
- [Kotlin实战：日期与时间的处理](https://www.geeksforgeeks.org/kotlin-date-time/)
# 参见
* [Java中的日期与时间](https://github.com/fei-huang/java-blog-posts/blob/main/Date%20and%20Time%20in%20Java.md)
* [Java中的format()方法和parse()方法](https://github.com/fei-huang/java-blog-posts/blob/main/Formatting%20and%20Parsing%20Dates%20in%20Java.md)