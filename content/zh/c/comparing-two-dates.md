---
title:                "C: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

无论是在日常生活还是在程序设计中，我们经常需要比较两个日期的先后顺序。比如，判断今天是不是某个重要日子，或者计算两个事件之间的时间差。在C编程中，比较两个日期也是非常常见的需求。因此，学习如何在C语言中比较两个日期是很重要的。

## 如何实现

比较两个日期的常用方法是通过将日期转换为整数，然后比较这两个整数的大小。下面是一个简单的例子，展示了如何在C语言中比较两个日期：

```C
#include <stdio.h>

int main()
{
    // 定义两个日期变量
    int date1 = 20200101;
    int date2 = 20210101;

    // 比较两个日期的大小
    if (date1 > date2)
    {
        printf("date1 大于 date2");
    }
    else if (date1 < date2)
    {
        printf("date1 小于 date2");
    }
    else
    {
        printf("date1 等于 date2");
    }

    return 0;
}
```

运行上面的代码，我们可以得到以下输出：

```
date1 小于 date2
```

通过将日期转换为整数，我们可以很容易地实现日期的比较。但是，如果我们直接比较日期变量的话，结果可能不太准确，尤其是当涉及闰年的情况时。因为每个月的天数不同，所以我们需要根据月份和闰年来确定每个月的天数。下面的代码展示了如何按照这种方法比较日期：

```C
#include <stdio.h>

// 判断是否为闰年
int isLeapYear(int year)
{
    // 普通平年，2月有28天
    if (year % 4 != 0)
    {
        return 0;
    }

    // 世纪闰年，但非400的倍数，2月有28天
    if (year % 100 == 0 && year % 400 != 0)
    {
        return 0;
    }

    // 符合以上两种情况，即为闰年
    return 1;
}

// 根据月份和是否为闰年来确定每个月的天数
int getDays(int month, int isLeapYear)
{
    // 默认每个月都有31天，除了2月
    int days = 31;

    // 根据月份来确定天数
    if (month == 4 || month == 6 || month == 9 || month == 11)
    {
        days = 30;
    }
    else if (month == 2)
    {
        // 根据是否是闰年来确定2月的天数
        days = isLeapYear ? 29 : 28;
    }

    return days;
}

int main()
{
    // 定义两个日期变量
    int date1 = 20200101;
    int date2 = 20210201;

    // 提取每个日期的年、月、日
    int year1 = date1 / 10000;
    int year2 = date2 / 10000;
    int month1 = (date1 % 10000) / 100;
    int month2 = (date2 % 10000) / 100;
    int day1 = date1 % 100;
    int day2 = date2 % 100;

    // 默认两个日期是相等的
    int result = 0;

    // 如果年份相同，比较月份，再比较天数
    if (year1 == year2)
    {
        if (month1 == month2)
        {
            result = (day1 > day2) ? 1 : (day1 < day2) ? -1 : 0;
        }
        else
        {
            result = (month1 > month2) ? 1 : -1;
        }
    }
    else
    {