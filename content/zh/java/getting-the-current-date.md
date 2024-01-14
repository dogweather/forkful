---
title:                "Java: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么

在编程的世界里，获取当前日期是一个普遍且有用的操作。无论你是在开发一个日历应用程序还是需要在日志记录中添加时间戳，都需要用到当前日期。在这篇博文中，我将向大家展示如何使用Java代码来获取当前日期，让我们一起来探索吧！

# 如何

要获取当前日期，我们需要使用Java中的内置类Date。我们可以通过创建一个Date对象来获取当前日期，然后使用SimpleDateFormat来格式化日期输出。

```Java
Date currentDate = new Date(); // 创建一个Date对象
SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd"); // 定义日期格式
String formattedDate = dateFormat.format(currentDate); // 格式化当前日期
System.out.println("今天的日期是：" + formattedDate); // 输出结果：今天的日期是：2021-05-26
```

除了获取当前日期，我们也可以使用SimpleDateFormat来格式化任何指定日期。例如，如果我们想要获取明天的日期，可以使用Calendar类来进行日期计算。

```Java
Calendar calendar = Calendar.getInstance(); // 创建Calendar对象
calendar.add(Calendar.DATE, 1); // 将日期推迟一天
Date nextDate = calendar.getTime(); // 获取推迟后的日期
String nextFormattedDate = dateFormat.format(nextDate); // 格式化日期输出
System.out.println("明天的日期是：" + nextFormattedDate); // 输出结果：明天的日期是：2021-05-27
```

# 深入探讨

除了上述示例中，我们还可以根据需要使用不同的日期格式来格式化输出结果。SimpleDateFormat类中提供了多种日期格式选项，例如“MM/dd/yyyy”表示月/日/年，“dd MMM yyyy”表示日 月 年，等等。您可以根据需要选择合适的日期格式来展示您想要的日期输出。

除了SimpleDateFormat类，Java 8之后还提供了新的日期和时间API，例如LocalDateTime类，它提供了更方便、更灵活的日期和时间操作方法。您可以深入了解这些API，以便根据需要选择最适合您项目的日期和时间处理方法。

# 参考资料

- [Java中的日期和时间处理](https://www.runoob.com/java/java-date-time.html)
- [SimpleDateFormat文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java 8新的日期和时间API](https://www.geeksforgeeks.org/java-8-localdatetime-class/)