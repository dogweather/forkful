---
title:                "计算未来或过去的日期"
html_title:           "C: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期是一种常见的编程任务，它可以让我们根据当前日期快速计算出未来或过去的日期。程序员经常需要这样做，尤其是在编写日历、日程安排或者时间管理软件时。

## 如何：
```
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm futureDate;
    struct tm pastDate;
    time_t t = time(NULL);
    localtime_s(&futureDate, &t);
    localtime_s(&pastDate, &t);

    printf("今天的日期: %02d/%02d/%d\n", futureDate.tm_mon + 1, futureDate.tm_mday, futureDate.tm_year + 1900);

    // 计算未来的日期
    futureDate.tm_mday += 7;
    mktime(&futureDate);
    printf("一周后的日期: %02d/%02d/%d\n", futureDate.tm_mon + 1, futureDate.tm_mday, futureDate.tm_year + 1900);

    // 计算过去的日期
    pastDate.tm_mday -= 7;
    mktime(&pastDate);
    printf("一周前的日期: %02d/%02d/%d\n", pastDate.tm_mon + 1, pastDate.tm_mday, pastDate.tm_year + 1900);

    return 0;
}
```
```
今天的日期: 09/19/2020
一周后的日期: 09/26/2020
一周前的日期: 09/12/2020
```

## 深入了解：
计算日期在现代计算中已经变得非常简单，但在过去的计算机时代，却需要更复杂的方法。在早期的操作系统中，日期是用一种称为“儒略日”的系统来存储的，它表示自 1582 年 10 月 15 日至今的天数。

在 C 中，日期计算也可以使用库函数中的 `time.h` 头文件来实现。除了我们展示的方法外，也可以使用 `strftime()` 函数来格式化日期。

## 参考资料：
- [C 中处理日期的标准库函数](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [如何在 C 中计算日期差](https://stackoverflow.com/questions/20427730/how-to-calculate-the-difference-between-two-dates-in-c-c)
- [儒略日历](https://zh.wikipedia.org/wiki/%E5%84%92%E7%95%A5%E6%97%A5)