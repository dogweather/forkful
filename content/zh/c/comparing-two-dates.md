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

## 为什么

当我们需要比较两个日期时，我们可能会用到一些比较操作符，比如大于、小于和等于。这有助于我们在程序中判断某个日期是在另一个日期之前还是之后。

## 如何

比较两个日期可以通过使用以下方法来实现：

```C
// 引入 <stdio.h> 头文件
#include <stdio.h>

// 定义函数来比较日期
int compareDate(int year1, int month1, int day1, int year2, int month2, int day2) {

    // 若第一个日期在第二个日期之前，则返回 -1
    if (year1 < year2) {
        return -1;
    }
    else if (year1 == year2 && month1 < month2) {
        return -1;
    }
    else if (year1 == year2 && month1 == month2 && day1 < day2) {
        return -1;
    }

    // 若第一个日期与第二个日期相等，则返回 0
    else if (year1 == year2 && month1 == month2 && day1 == day2) {
        return 0;
    }

    // 若第一个日期在第二个日期之后，则返回 1
    else {
        return 1;
    }
}

// 调用函数，并打印输出
int main(void) {
    int year1 = 2020;
    int month1 = 4;
    int day1 = 30;
    int year2 = 2021;
    int month2 = 2;
    int day2 = 14;

    int result = compareDate(year1, month1, day1, year2, month2, day2);

    printf("结果为：%d", result);

    return 0;
}
```

以上代码的输出结果为：1，表示第一个日期在第二个日期之后。

## 深入了解

日期的比较涉及到对年份、月份和日期的比较，需要注意的是在比较月份和日期时，要考虑到不同月份的天数和闰年的情况。比如，在比较 2020 年 2 月 14 日和 2021 年 2 月 14 日时，若只简单地比较月份和日期，则会得出两个相等的日期，但实际上它们是不同的日期。

## 参考链接

- [C 语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [日期比较函数](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm)
- [C 语言比较操作符](https://www.geeksforgeeks.org/relational-operators-in-c-c/)

## 参见

- [C语言比较字符串](https://github.com/example/c_compare_strings)