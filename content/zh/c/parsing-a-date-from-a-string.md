---
title:                "从字符串中解析日期"
html_title:           "C: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串解析及为何程序员需要它？
字符串解析是将字符串中的日期信息提取出来，以便程序员可以对日期进行操作和处理。程序员常常需要解析日期字符串来满足不同的需求，比如将日期格式转换为指定格式、计算两个日期之间的间隔等。

## 如何进行字符串解析？
字符串解析需要使用C的日期和时间库`<time.h>`中的函数来实现。下面是一个示例代码，以及其输出结果：

```C
#include <stdio.h>
#include <time.h>

int main()
{
    char date_string[] = "2021/10/21 12:30:45";
    char format[] = "%Y/%m/%d %H:%M:%S";
    struct tm tm;
    strptime(date_string, format, &tm);
    printf("Year: %d, Month: %d, Day: %d, Hour: %d, Minute: %d, Second: %d\n",
           tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
    return 0;
}

```

输出结果为：
> Year: 2021, Month: 10, Day: 21, Hour: 12, Minute: 30, Second: 45

## 深入了解
字符串解析在早期的编程语言中是一个非常常见的操作，因为那时没有专门的日期和时间类型，日期信息都存储在字符串中。如今，大多数编程语言都提供了自己的日期和时间数据类型，可以直接操作日期，而无需解析字符串。但是，在某些情况下，仍然需要解析字符串，比如从数据库中读取日期信息。如果不熟悉日期和时间库的用法，还可以使用字符串解析来方便处理。

## 参考资料
- [C语言日期和时间函数](https://www.runoob.com/cprogramming/c-data-time.html)
- [日期解析和格式化库strptime、strftime的用法](https://www.cnblogs.com/guoguijuan/p/6125617.html)