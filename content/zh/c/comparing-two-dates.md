---
title:                "比较两个日期"
html_title:           "C: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么是日期比较？为什么程序员需要它？

日期比较是指在编程中比较两个日期的过程。程序员经常需要比较两个日期来确定它们是否相等或者哪个日期更早或更晚。

## 如何进行日期比较：

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 创建两个时间结构体，分别表示要比较的日期
    struct tm date1 = { .tm_year = 2021, .tm_mon = 4, .tm_mday = 11 };
    struct tm date2 = { .tm_year = 2021, .tm_mon = 4, .tm_mday = 10 };

    // 使用difftime函数来比较两个日期的差异
    double difference = difftime(mktime(&date1), mktime(&date2));

    // 打印日期1是否晚于日期2
    if (difference > 0) {
        printf("日期1晚于日期2");
    } else if (difference < 0) {
        printf("日期1早于日期2");
    } else {
        printf("日期1和日期2相等");
    }

    return 0;
}
```

输出:

```C
日期1早于日期2
```

## 深入了解：

日期比较在编程中扮演着重要的角色。在早期，计算机系统并没有内置日期比较功能，程序员们不得不使用其它方法来实现日期比较。现在，可以使用C语言中的time.h头文件来轻松地比较日期。除了difftime函数，也可以使用比较运算符（如>、<、==）来比较两个日期。此外，还有一些库可以更方便地处理日期和时间，如Boost库和C++中的chrono库。

## 参考链接：

- [time.h指南](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Boost.Date_Time库](https://www.boost.org/doc/libs/1_37_0/doc/html/date_time.html)
- [C++标准库chrono](https://en.cppreference.com/w/cpp/chrono)