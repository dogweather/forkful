---
title:                "Java: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##为什么

你是否曾经想要编写程序来计算未来或过去的日期？这可能对于预订旅行或安排日程非常有用。在本篇博客文章中，我将通过简单的Java编程示例向你展示如何做到这一点。

##如何进行

首先，我们需要使用Java提供的日期类。以下是一个简单的Java程序，用于计算明天的日期，并输出结果。

```Java
import java.time.LocalDate;

public class DateCalculator {
    public static void main(String[] args) {
        //获取当前日期
        LocalDate date = LocalDate.now();
        //使用plusDays方法计算明天的日期
        LocalDate tomorrow = date.plusDays(1);
        //输出结果
        System.out.println("明天的日期是：" + tomorrow);
    }
}
```

运行该程序将输出如下结果：

```
明天的日期是：2021-06-01
```

我们可以看到，使用Java提供的plusDays方法，我们可以轻松地计算出未来日期。

除了计算未来日期，我们也可以计算过去日期。以下是一个示例程序，用于计算昨天的日期。

```Java
import java.time.LocalDate;

public class DateCalculator {
    public static void main(String[] args) {
        //获取当前日期
        LocalDate date = LocalDate.now();
        //使用minusDays方法计算昨天的日期
        LocalDate yesterday = date.minusDays(1);
        //输出结果
        System.out.println("昨天的日期是：" + yesterday);
    }
}
```

运行该程序将输出如下结果：

```
昨天的日期是：2021-05-30
```

如此简单，我们就可以通过Java编程来计算未来或过去的日期。

##深入了解

除了使用plusDays和minusDays方法，Java还提供了其他许多方便的方法来计算日期。例如，我们可以使用plusWeeks和minusWeeks方法来计算未来或过去的周数。同时，我们还可以使用isBefore和isAfter方法来比较两个日期的先后顺序。

另外，如果你想要指定一个特定的日期，那么可以使用of方法来创建一个LocalDate对象。以下是一个示例程序，用于指定一个特定的日期并输出结果。

```Java
import java.time.LocalDate;

public class DateCalculator {
    public static void main(String[] args) {
        //使用of方法指定一个日期
        LocalDate date = LocalDate.of(2021, 6, 1);
        //输出结果
        System.out.println("指定的日期是：" + date);
    }
}
```

运行该程序将输出如下结果：

```
指定的日期是：2021-06-01
```

需要注意的是，月份和日期都是从1开始计数的。

##另请参阅

- [Java官方文档 - LocalDate类](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [菜鸟教程 - Java日期时间](https://www.runoob.com/java/java-date-time.html)
- [Java八码 - 如何用Java计算日期](https://javapedia.io/how-to-calculate-dates-using-java/)