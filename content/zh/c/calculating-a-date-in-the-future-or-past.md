---
title:                "C: 计算未来或过去的日期"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

在日常生活中，我们经常需要计算未来或者过去的日期，比如预订旅行日期或者登记重要的生日。这时候，使用C语言来帮助我们计算日期就非常有用了。

## 为什么
计算日期可以帮助我们更有效地组织生活。通过C语言，我们可以轻松地计算出未来或者过去任意一天的日期，无论是一天，一个月，还是多年。

## 如何
首先，我们需要了解日期的基本概念。日期由年，月，日组成，可以用整数来表示。接着，我们需要使用C语言的日期函数来计算日期，比如`mktime()`和`gmtime()`。以下是一个简单的例子，计算今天的日期：

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now;
    time(&now);
    struct tm *today = localtime(&now);
    
    int year = today->tm_year + 1900; // 获取当前年份
    int month = today->tm_mon; // 获取当前月份
    int day = today->tm_mday; // 获取当前日期
    
    printf("今天的日期是：%d年%d月%d日\n", year, month, day);
    
    return 0;
}
```

这段代码中，我们使用了`time()`函数获取当前时间，并将其赋值给`now`变量。然后，使用`localtime()`函数将`now`转换为本地时间，并存储在`today`结构体指针中。接着，我们可以通过`today`指针来获取当前的年，月，日信息。最后，使用`printf()`函数将日期打印出来。

如果要计算未来或者过去的日期，我们可以使用`mktime()`函数来将日期转换为秒数，再根据需求进行加减。以下是一个计算未来日期的例子：

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm future = {0}; // 初始化一个tm结构体
    future.tm_year = 2021 - 1900; // 将年份设置为2021
    future.tm_mon = 11; // 将月份设置为12月
    future.tm_mday = 25; // 将日期设置为25日
    
    time_t future_time = mktime(&future); // 将日期转换为秒数
    future_time += 86400; // 将未来日期后移一天
    struct tm *new = localtime(&future_time); // 将秒数转换为本地时间
    
    int year = new->tm_year + 1900; // 获取年份
    int month = new->tm_mon + 1; // 获取月份
    int day = new->tm_mday; // 获取日期
    
    printf("未来日期为：%d年%d月%d日\n", year, month, day);
    
    return 0;
}
```

这段代码中，我们首先使用`struct tm`结构体初始化了一个`future`变量，并将其设置为2021年12月25日。然后，使用`mktime()`函数将日期转换为秒数，并将其存储在`future_time`变量中。接着，我们将`future_time`加上一天（86400秒），并使用`localtime()`函数将秒数转换为本地时间。最后，我们可以通过指针`new`来获取未来日期的年，月，日信息，并将其打印出来。

## 深入探讨
在C语言中，日期的计算涉及到许多复杂的概念，比如闰年的判断，每月天数的变化等等。为了更深入地了解日期的计算，建议阅读相关的文档或者教程。

## 参考资料
- [C日期和时间函数](https://www.runoob.com/cprogramming/c-standard-library-time-h.html)
- [了解mktime()函数](https://www.geeksforgeeks.org/mktime-function