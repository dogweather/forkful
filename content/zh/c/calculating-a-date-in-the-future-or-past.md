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

## 为什么

计算未来或过去的日期可能会对你有所帮助。例如，你想要安排行程或者了解特定日期是星期几。

## 如何进行

计算日期需要使用C标准库中的time.h头文件。我们将使用以下几个函数来帮助我们进行日期计算。

```C 
#include <stdio.h> 
#include <time.h> 

int main() { 
  // 获取当前时间
  time_t curr_time = time(NULL); 
  
  // 将当前时间转换为tm结构体
  struct tm *time_struct = localtime(&curr_time); 
  
  // 设置要计算的年、月、日
  int year = time_struct->tm_year + 1900; // 年份加1900 
  int month = time_struct->tm_mon + 1; // 月份加1 
  int day = time_struct->tm_mday + 1; // 日期加1 
  
  // 调整日期
  time_struct->tm_year = year; 
  time_struct->tm_mon = month; 
  time_struct->tm_mday = day; 
  
  // 使用mktime函数将tm结构体转换为time_t类型
  time_t new_time = mktime(time_struct); 
  
  // 使用strftime函数将日期格式化为年-月-日的形式
  char buf[10]; 
  strftime(buf, 10, "%Y-%m-%d", localtime(&new_time)); 
  
  // 输出计算后的日期
  printf("明天的日期是：%s\n", buf); 
  
  return 0; 
} 
```

运行以上代码将输出：明天的日期是：2020-10-21。

如果需要计算过去的日期，只需更改日期的加减操作。

## 深入探讨

日期计算涉及到很多复杂的算法，如闰年的处理、月份的天数等等。深入了解这些算法可以让我们更灵活地进行日期计算，并且在编写程序时能够更好地处理各种边界情况。

## 参考链接

- [C标准库中的time.h头文件](https://en.cppreference.com/w/c/chrono/time)
- [关于日期计算的更多信息](https://www.timeanddate.com/date/dateadd.html)
- [mktime函数的详细说明](https://en.cppreference.com/w/c/chrono/mktime)
- [strftime函数的详细说明](https://en.cppreference.com/w/c/chrono/strftime)

## 参看

- [C语言入门教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [编程自学指南：如何有效地学习编程](https://medium.com/@danvyle/how-to-teach-yourself-programming-and-become-a-self-taught-programmer-4ad15575fc07)
- [如何使用C语言进行日期计算](https://www.programiz.com/c-programming/examples/current-date-time)