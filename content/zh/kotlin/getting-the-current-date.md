---
title:                "获取当前日期"
html_title:           "Kotlin: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 什么是获取当前日期？为什么程序员要这么做？

获取当前日期是指从计算机系统中获取当前的日期和时间的操作。程序员经常需要使用当前日期来记录程序的创建时间、交易时间或者记录事件发生的时间。因此，获取当前日期对于程序员来说是非常重要的。

# 如何获取当前日期？

```
Kotlin val currentDate = LocalDate.now()
```

这个简单的代码可以帮助你获取当前的日期。你也可以使用其他的日期格式，例如：

```
Kotlin val currentDate = LocalDateTime.now()
val formattedDate = currentDate.format(DateTimeFormatter.BASIC_ISO_DATE)
```

这样可以将当前日期转换为基本的ISO格式，也就是“yyyymmdd”。

# 深入了解

## 历史背景

在编程的早期，程序员们需要手动编写代码来获取当前日期。随着计算机技术的发展，现在已经有很多内置的函数和库来帮助程序员简便地获取当前日期。

## 其他方法

除了使用内置函数来获取当前日期，程序员还可以使用第三方库。例如，Joda-Time是用于处理日期和时间的流行开源库。

## 实现细节

在计算机系统中，时间是以整数来表示的。从1970年1月1日开始，每秒钟会增加一个整数值，这就是我们所说的“时间戳”。程序员可以通过获取时间戳来计算当前日期。

# 参考文献

- [Java Doc - LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda-Time Docs](https://www.joda.org/joda-time/)
- [Timestamps Explained](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-java)