---
date: 2024-01-20 17:31:20.200269-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Java 8\u4E4B\u524D\uFF0C\u65E5\u671F\
  \u548C\u65F6\u95F4\u662F\u901A\u8FC7`java.util.Date`\u548C`java.util.Calendar`\u5904\
  \u7406\u7684\u3002Java\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.959316-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

## 如何操作：
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        LocalDate tenDaysLater = today.plusDays(10);
        LocalDate threeWeeksEarlier = today.minusWeeks(3);

        System.out.println("Today: " + today);
        System.out.println("Ten days later: " + tenDaysLater);
        System.out.println("Three weeks earlier: " + threeWeeksEarlier);
    }
}
```
输出：
```
Today: 2023-03-30
Ten days later: 2023-04-09
Three weeks earlier: 2023-03-09
```

## 深入了解
在Java 8之前，日期和时间是通过`java.util.Date`和`java.util.Calendar`处理的。Java 8引入了`java.time`包，提供了更简洁、更直观的API。除了直接加减天数、周数、月数等，你还可以使用`ChronoUnit`来计算两个日期之间的差距。如果你需要更复杂的日期操作，可以考虑第三方库如Joda-Time，但自从Java 8发行以来，需要第三方库的情况已经大大减少。

## 参考资料
- [LocalDate (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [ChronoUnit (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/time/temporal/ChronoUnit.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
