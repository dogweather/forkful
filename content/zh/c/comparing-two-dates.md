---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么?

将两个日期进行比较就是确定两个日期在时间上的先后顺序。程序员之所以要进行日期的比较，主要是为了在计算日期差，排序事件，或是在一些条件语句中进行日期的对比。

## 如何操作:

这是一个简单的C语言程序，可让你了解如何比较两个日期：

```C
#include <stdio.h>

typedef struct Date {
    int day;
    int month;
    int year;
} Date;

// this function returns -1 if date1 < date2, 1 if date1 > date2 and 0 if dates are same
int compareDates(Date date1, Date date2) {
    // compare years
    if (date1.year < date2.year) return -1;
    if (date1.year > date2.year) return 1;

    // compare months
    if (date1.month < date2.month) return -1;
    if (date1.month > date2.month) return 1;

    // compare days
    if (date1.day < date2.day) return -1;
    if (date1.day > date2.day) return 1;

    return 0;
}

int main() {
    Date date1 = {25, 2, 2020};
    Date date2 = {13, 1, 2020};

    int result = compareDates(date1, date2);
    
    if(result == 1)
        printf("Date1 is later than Date2");
    else if (result == -1)
        printf("Date1 is earlier than Date2");
    else 
        printf("Both dates are same");
    
    return 0;
}
```

这个程序的输出会是 "Date1 is later than Date2"，因为在这个例子中，日期1晚于日期2。

## 深入学习:

这种比较日期的方法源自于Julius Caesar的儒略历，其日期是从年份开始计算的，这使得我们能够比较两个日期。注意，我们并没有处理闰年的情况，这需要在实际程序中考虑。

虽然我们直接使用C语言的结构体来存储日期，但有更多的方式可以处理日期。例如，你可以使用 `time_t` 类型或者 `struct tm` ，它们都是标准库 `time.h` 的一部分。

如果你需要比较的日期范围超出了一天，你可能需要第三方库，比如 `libdate`。这个库允许你比较日期并包括了闰年和夏令时等更多情况的处理。

## 参考资料:

1. [C library function - time()](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm) - 更多有关 C 库函数 time() 的信息。
2. [C programming dealing with Date and Time](https://aticleworld.com/c-programming-dealing-with-date-and-time) - 如何在 C 语言中处理日期和时间。
3. [Time Manipulation in C using time.h](https://www.geeksforgeeks.org/time-manipulation-in-c-using-tm-structure) - 使用 C 的 time.h 进行时间操作。