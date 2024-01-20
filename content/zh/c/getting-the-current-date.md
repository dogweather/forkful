---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是一个让计算机显示现在的年、月、日和时间的过程。这对于记录事件发生的时间、计算日期差、生成时间戳等任务至关重要。

## 如何实现：
以下代码演示了在C语言程序中获取并显示当前日期和时间的方法：
```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t t = time(NULL);
  struct tm *tm = localtime(&t);
  
  printf("当前日期和时间: %s", asctime(tm));  
  return 0;
}
```
运行此程序，你将看到如下输出（日期和时间会根据你运行该程序的时间有所变化）：
```C
当前日期和时间: Mon May 31 12:34:56 2021
```

## 深入解析
1. **历史背景**：虽然时钟和日历似乎与计算机科学无关，但它们在计算机程序中却发挥着重要作用，尤其是在需要跟踪和比较事件的应用中。

2. **替代方案**：除了用C标准库中的`time.h`来获取日期和时间，我们还可以使用POSIX函数，或是利用操作系统提供的API来完成这项任务。

3. **实现细节**：`time()`函数获取自1970/1/1 00:00 UTC以来的秒数，并通过`localtime()`函数转换为本地时区的时间。`asctime()`函数则是将时间结构转换为字符串。

## 另见
- [C语言时间日期函数](http://www.runoob.com/cprogramming/c-standard-library-time-h.html)
- [关于UNIX时间戳](https://en.wikipedia.org/wiki/Unix_time)
  
请注意，本文中的示例代码，其运行结果可能因操作系统、时区和本地化设置的不同而有所差异。