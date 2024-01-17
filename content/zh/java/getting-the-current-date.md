---
title:                "获取当前日期"
html_title:           "Java: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

获取当前日期是指获取当前时间的代码。程序员这样做是因为日期和时间在编程中是非常重要的，它们可用于跟踪程序的执行时间，以及在日志记录和数据分析中起到关键作用。

## 如何：

使用Java可以轻松获取当前日期。只需使用内置的Java类“java.util.Date”，然后调用“Date（）”方法就可以了。让我们看一个简单的例子：

```
import java.util.Date;
public static void main(String args[]) {
Date currentDate = new Date();
System.out.println(currentDate);
}
```

输出应该是类似于这样的结果：

```
Sat Feb 22 20:25:45 EST 2020
```

## 深入探讨：

在历史上，获取当前日期使用的是简单的数学运算。然而，在Java中，使用内置的“java.util.Date”类可以更加方便和精确地获取当前日期。另外，还有一些替代方法可以获取当前日期，比如使用“java.time.LocalDate”类，它在Java 8中被引入。

在实现时，我们需要注意的是获取的当前日期和时间是基于系统的本地时区。如果需要特定的时区，可以使用“java.time.ZoneDateTime”类来获取。

## 参考链接：

- [Java中的日期和时间](https://www.w3schools.com/java/java_date.asp)
- [Java 8中的新日期和时间API](https://www.baeldung.com/java-8-date-time-intro)