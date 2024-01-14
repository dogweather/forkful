---
title:    "Java: 获取当前日期"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 为什么

获取当前日期在Java程序中是一个常见的需求。它可以帮助我们记录程序运行的时间，比如记录日志，或者判断某个操作是否已经超时。在本文中，我们将深入讨论如何在Java中获取当前日期，并给出一些示例代码和输出结果。

# 如何做

要获取当前日期，我们需要使用Java自带的java.util包中的Date类。首先，我们需要在我们的代码中导入这个类，并使用它的无参构造函数来创建一个Date对象。然后，我们可以使用Date类的一些方法来获取当前日期的各个部分，比如年、月、日、小时、分钟等。

```Java
import java.util.Date;

// 创建Date对象
Date currentDate = new Date();
// 获取当前年份
int year = currentDate.getYear() + 1900;
// 获取当前月份
int month = currentDate.getMonth() + 1;
// 获取当前日期
int day = currentDate.getDate();
// 获取当前小时
int hour = currentDate.getHours();
// 获取当前分钟
int minute = currentDate.getMinutes();
```

我们也可以使用DateFormat类来格式化日期，并将其以指定的字符串格式输出。

```Java
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

// 创建SimpleDateFormat对象，指定日期格式
DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
// 格式化当前日期
String formattedDate = dateFormat.format(currentDate);
// 输出结果：2021-09-14 15:20:30
System.out.println(formattedDate);
```

# 深入讨论

在上面的示例中，我们使用了Date类和DateFormat类来获取和格式化当前日期。但是，据说在Java 8及以后的版本中，推荐使用java.time包中的LocalDateTime类来处理日期，因为它提供了更多的功能和更好的线程安全性。下面是使用LocalDateTime类的示例代码。

```Java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

// 创建DateTimeFormatter对象，指定日期格式
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
// 创建LocalDateTime对象，表示当前日期
LocalDateTime currentDateTime = LocalDateTime.now();
// 格式化当前日期
String formattedDateTime = currentDateTime.format(formatter);
// 输出结果：2021-09-14 15:20:30
System.out.println(formattedDateTime);
```

另外，我们也可以使用DateTimeFormatter类的其他方法来获取当前日期的各个部分，比如年、月、日、小时、分钟等。

# 参考链接

- [Java Date类文档](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat类文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java LocalDateTime类文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java DateTimeFormatter类文档](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

# 参见

- [Java教程 | 获取当前日期和时间](https://www.runoob.com/java/java-examples.html)
- [如何在Java 8中获取当前日期和时间](https://www.baeldung.com/java-8-date-time-intro)