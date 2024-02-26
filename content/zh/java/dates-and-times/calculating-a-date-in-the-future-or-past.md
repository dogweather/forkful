---
date: 2024-01-20 17:31:20.200269-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u627E\u51FA\u5728\u7ED9\u5B9A\u65E5\u671F\u524D\u540E\u67D0\u5929\u7684\u65E5\u671F\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u9884\u5B9A\
  \u4E8B\u4EF6\uFF0C\u8BA1\u7B97\u622A\u6B62\u65E5\u671F\uFF0C\u6216\u7BA1\u7406\u65E5\
  \u5386\u4E8B\u4EF6\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.203405-07:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u5C31\u662F\
  \u627E\u51FA\u5728\u7ED9\u5B9A\u65E5\u671F\u524D\u540E\u67D0\u5929\u7684\u65E5\u671F\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u9884\u5B9A\
  \u4E8B\u4EF6\uFF0C\u8BA1\u7B97\u622A\u6B62\u65E5\u671F\uFF0C\u6216\u7BA1\u7406\u65E5\
  \u5386\u4E8B\u4EF6\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期就是找出在给定日期前后某天的日期。程序员这么做是为了处理预定事件，计算截止日期，或管理日历事件。

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
