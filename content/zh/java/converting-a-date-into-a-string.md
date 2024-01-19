---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

将日期转换成字符串是数据表现方法的改变，通常在输出用户需要查看或存储的日期信息时需要进行此操作。

## 怎么做：

下面的Java代码示例演示如何将“日期”对象转换为字符串。

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        // 创建一个日期对象
        Date date = new Date();

        // 创建一个日期格式化对象
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");

        // 将日期转换为字符串
        String strDate = formatter.format(date);

        // 打印字符串日期
        System.out.println("String format of the date: " + strDate);
    }
}
```

在运行上述程序，你将看到像这样的输出：

```Java
String format of the date: 23/04/2022
```

## 深度剖析：

Java日期转换为字符串的处理始于Java早期，开发者开始需要以人类可读的格式显示和记录日期信息。虽然有许多方式可以实现，使用SimpleDateFormat通常是最直接的。

如果你使用Java 8及以上版本，可以选择使用DateTimeFormatter，这是一个新的日期处理API，性能更优，线程安全。

当你转换日期到字符串时，你需要选择合适的格式。例如，"dd/MM/yyyy"代表了“日/月/年”的格式。

## 另请参阅：

以下链接提供了更多关于Java日期与字符串相互转换的信息：

1. Oracle Java Documentation: Class SimpleDateFormat - [link](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
2. Oracle Java Documentation: Class DateTimeFormatter - [link](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
3. TutorialsPoint - Java Date & Time - [link](https://www.tutorialspoint.com/java/java_date_time.htm)