---
title:                "请提供《获取当前日期》"
html_title:           "Java: 请提供《获取当前日期》"
simple_title:         "请提供《获取当前日期》"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

人们经常需要获取当前日期，无论是记录事件、处理报表还是进行数据分析。Java提供了简单易用的方法来获取当前日期，并且可以根据自己的需要进行格式化输出。

## 如何获取当前日期

获取当前日期的方法很简单，只需要使用Java内置的Date类和SimpleDateFormat类即可。首先，我们需要导入这两个类：

```Java
import java.util.Date;
import java.text.SimpleDateFormat;
```
然后，我们可以创建一个Date对象并使用SimpleDateFormat类来格式化输出日期，例如：

```Java
Date currentDate = new Date();
SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
String formattedDate = dateFormat.format(currentDate);
System.out.println("当前日期是：" + formattedDate);
```
运行以上代码，您将会得到类似于以下的输出：

```
当前日期是：2021-06-08
```

我们还可以根据自己的需要，使用不同的日期格式来输出，例如：

```Java
SimpleDateFormat longDateFormat = new SimpleDateFormat("yyyy年MM月dd日 HH时mm分ss秒");
String longFormattedDate = longDateFormat.format(currentDate);
System.out.println("当前日期和时间是：" + longFormattedDate);
```
输出结果为：

```
当前日期和时间是：2021年06月08日 15时29分37秒
```

除了日期以外，我们还可以获取当前时间的毫秒数，示例如下：

```Java
long currentTimeMillis = System.currentTimeMillis();
System.out.println("当前时间的毫秒数是：" + currentTimeMillis);
```
输出结果为：

```
当前时间的毫秒数是：1623163286203
```

## 深入了解

Java中Date类提供了许多方法来操作日期，例如可以比较两个日期的大小、判断一个日期是否在另一个日期之前或之后。在Java 8及以上版本，还可以使用新的LocalDate类来处理日期，它提供了更多的方法来操作日期和时间。

另外，Java提供了众多的第三方日期库，如Joda-Time、java.time和Threeten Extra，它们提供了更多的功能和更丰富的API来处理日期。

## 参考链接

- [Java Date类文档](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat类文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java LocalDate类文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda-Time官方文档](https://www.joda.org/joda-time/)
- [java.time官方文档](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- [Threeten Extra官方文档](https://www.threeten.org/threeten-extra/)

## 参见

- [Java教程（中文版）](https://docs.oracle.com/javase/tutorial/index.html)
- [在线Java编译器](https://www.jdoodle.com/online-java-compiler)
- [Java之道](https://www.javacodegeeks.com/)