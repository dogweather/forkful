---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
解析一个日期从字符串就是从文本形式的日期信息中提取出实践上可用的日期数据。程序员之所以需要这样做，是因为它可以使得程序逻辑更加言简意赅，更适合数据的使用和管理。

## 如何操作：
```Kotlin
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.Date
  
fun main(args: Array<String>) {
   val string = "2022-04-13"
   val date = SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH).parse(string)
   println(date)
}
```
样本输出:
```Kotlin
Wed Apr 13 00:00:00 CST 2022
```
## 深入探索：
在计算机程序的早期历史中，为了节省存储空间，日期和时间通常存储为字符串。但随着时间的推移，人们发现这种方法在处理某些日期或者时间的问题时，比如计算日期差或者日期排序等等是非常不方便的，因此人们开始将日期和时间解析并表示成可以进行各种操作的日期对象。

至于选择哪种解析，取决于问题的实际需要。例如，可以使用SimpleDateFormat类的parse()方法，也可以用LocalDate类的parse() 方法，或者你也可以自定义一个用于解析字符串中的日期的函数。

在使用SimpleDateFormat执行日期解析时，如果为不可解析的字符串提供日期格式，那么会抛出ParseException。为了处理这一问题，你需要使用try...catch结构来捕获异常。

## 另请参阅：
1. Kotlin官方文档关于日期和时间处理的部分: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/
2. Java SimpleDateFormat官方文档: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
3. Oracle关于日期时间API的教程: https://docs.oracle.com/javase/tutorial/datetime/index.html