---
title:                "Java: 将日期转换为字符串"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：有多种原因可以将日期转换为字符串。例如，您可能需要将日期格式化为特定的字符串格式，或者您可能需要将日期存储或传递给其他系统。

如何：在Java中，通过使用SimpleDateFormat类可以将日期转换为字符串。您可以创建一个SimpleDateFormat对象，并使用其format（）方法来格式化日期。下面是一个简单的代码示例，演示如何将日期格式化为yyyy-MM-dd格式的字符串：

```Java
SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
Date date = new Date();
String formattedDate = dateFormat.format(date);
System.out.println(formattedDate); // 输出：2020-09-15
```

深入探讨：在上面的示例中，通过指定格式字符串“yyyy-MM-dd”来创建SimpleDateFormat对象，该对象可以将日期转换为该格式的字符串。您还可以使用其他格式字符串来获得不同的日期格式。此外，您可以使用SimpleDateFormat类的parse（）方法来将字符串转换回日期对象。

同时，值得注意的是，在多线程环境中，SimpleDateFormat类是不安全的，因此建议使用线程安全的DateTimeFormatter类来执行日期转换操作。

另外，您还可以使用Calendar类来获取日期的特定部分，例如年、月、日等。

## 查看更多资料

- [Java SimpleDateFormat文档](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java多线程环境下的日期格式化问题](https://stackoverflow.com/questions/37948438/how-to-format-dates-in-java-like-simpledateformat-but-synchronized)
- [Java Calendar文档](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java DateTimeFormatter文档](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)