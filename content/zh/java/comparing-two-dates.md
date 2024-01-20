---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

日期比较指的是计算两个不同日期之间的差别，包括年、月、日、小时、分钟和秒。编程者之所以进行日期比较，是因为它可以方便地管理和操作日期数据，执行计划任务，内容丰富，例如可能是为了确定一个事件是否已经发生，或者在给定的时间范围内是否发生。

## 如何做？

下面的代码示例描述了如何使用Java的LocalDate类的isBefore(), isAfter()和isEqual()方法比较两个日期：

```Java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2020, 1, 1);
        LocalDate date2 = LocalDate.of(2021, 1, 1);

        if (date1.isBefore(date2)) {
            System.out.println("Date1 is before Date2");
        }

        if (date1.isAfter(date2)) {
            System.out.println("Date1 is after Date2");
        }

        if (date1.isEqual(date2)) {
            System.out.println("Date1 is equal to Date2");
        }
    }
}
```
输出结果是 : 
```
Date1 is before Date2
```

## 深入研究

在历史的长河中，我们已经从Java.util.Date中的比较方法（通过.equals()和.compareTo()方法）发展到现在更简单，更直接的LocalDate的比较方法。旧方法虽然适用，但在处理闰年和时区时可能会遇到问题，而新方法则可以轻松处理这些问题。

至于实现细节，isBefore(), isAfter()和isEqual()方法通过比较年、月、日的值来判断日期的顺序和相等性。

另外，通过使用比较方法之外，还有一种代替方法，那就是用Period类计算两个日期之间的天数，月数或年数差。

```Java
import java.time.LocalDate;
import java.time.Period;

public class DateDiff {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2020, 1, 1);
        LocalDate date2 = LocalDate.of(2021, 1, 1);

        Period period = Period.between(date1, date2);

        System.out.printf("Differences: %d years, %d months, %d days \n", 
        period.getYears(), period.getMonths(), period.getDays());
    }
}
```
输出结果是 : 
```
Differences: 1 years, 0 months, 0 days 
```
## 参见

- [Java LocalDate文档](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Period文档](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)
- [Java官方教程：Date-Time API](https://docs.oracle.com/javase/tutorial/datetime/index.html)