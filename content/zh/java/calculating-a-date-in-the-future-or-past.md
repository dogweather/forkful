---
title:                "计算未来或过去的日期"
html_title:           "Java: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么

在编写Java代码时，经常会遇到需要计算将来或过去日期的情况。这种日期计算能够为我们提供更便捷的时间处理方式，使得我们的程序更加智能化和灵活性。因此，学习如何在Java中计算日期是非常有用的技能。

# 如何做

首先，我们需要了解Java中的日期和时间类库。Java提供了许多方便的日期时间处理类，如`LocalDate`和`LocalDateTime`。我们可以使用这些类来创建日期对象，并进行日期计算。给定一个特定的日期，我们可以使用`plus()`和`minus()`方法来计算未来或过去的日期。下面是一个简单的示例代码，用于计算未来5天后的日期：

```Java
// 导入日期相关类库
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculation {

    public static void main(String[] args) {
        // 定义要计算的日期
        String dateString = "2021-07-10";

        // 将字符串转换为日期对象
        LocalDate date = LocalDate.parse(dateString, DateTimeFormatter.ISO_DATE);

        // 使用plus()方法计算未来日期
        LocalDate futureDate = date.plusDays(5);

        // 将结果打印出来
        System.out.println("未来5天后的日期为：" + futureDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
    }
}
```

运行上面的代码，输出结果为："未来5天后的日期为：2021-07-15"。

类似地，我们也可以使用`minus()`方法来计算过去的日期。下面是一个示例代码，用于计算过去1个月前的日期：

```Java
// 导入日期相关类库
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculation {

    public static void main(String[] args) {
        // 定义要计算的日期
        String dateString = "2021-07-10";

        // 将字符串转换为日期对象
        LocalDate date = LocalDate.parse(dateString, DateTimeFormatter.ISO_DATE);

        // 使用minus()方法计算过去日期
        LocalDate pastDate = date.minusMonths(1);

        // 将结果打印出来
        System.out.println("过去1个月前的日期为：" + pastDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
    }
}
```

运行上面的代码，输出结果为："过去1个月前的日期为：2021-06-10"。

# 深入了解

以上示例代码只展示了如何使用`LocalDate`类来计算日期，但实际上Java还提供了许多其他的日期时间处理类，如`Instant`、`ZonedDateTime`和`Period`等。每个类都有其特定的用途，可以根据实际需求选择使用。此外，Java 8以后还引入了`java.time.temporal.TemporalAmount`接口，可以用来代替`plus()`和`minus()`方法来进行日期计算，更加灵活方便。

# 参考

1. Java 8日期处理：https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
2. Java 8日期文档：https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
3. Java日期时间类库介绍：https://www.baeldung.com/java-date-time-api