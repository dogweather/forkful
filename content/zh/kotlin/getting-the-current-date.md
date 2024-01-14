---
title:                "Kotlin: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

为什么要获取当前日期？在日常生活和编程中，我们经常需要对时间进行操作，比如记录某事件发生的时间、根据时间做出决策等等。因此，获取当前日期是十分重要的。

## 如何

在Kotlin中，获取当前日期有多种方法。下面将介绍两种常用的方法，并附上对应的代码和输出结果。

```
// 使用java.util包下的Date类
val currentDate = Date()
println(currentDate)

// 输出结果：Mon Aug 30 14:56:08 CST 2021
```

```
// 使用java.time包下的LocalDate类
val currentDate = LocalDate.now()
println(currentDate)

// 输出结果：2021-08-30
```

值得注意的是，使用java.time包需要Kotlin版本在1.8以上。

## 深入

在深入了解获取当前日期的过程中，我们需要了解一些概念。在计算机中，日期通常以自公元1970年1月1日00:00:00距离当时的秒数来表示，这被称为Epoch Time。因此，我们可以通过`Date()`的构造函数来获取自Epoch Time以来的秒数，从而得到当前日期。

在Kotlin中，使用Java Date和Java Time API都是十分简单的方法来获取当前日期。根据实际需求，可以选择使用不同的API来满足需求。

## 参考链接

- [Kotlin官方文档](https://kotlinlang.org/docs/home.html)
- [Java Date类的使用](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java Time API的使用](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Epoch Time介绍](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date/now)