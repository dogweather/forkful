---
title:    "C: 获取当前日期"
keywords: ["C"]
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在编写软件时，经常需要获取当前日期作为程序的一部分。例如，如果您正在编写一个日历应用程序，那么您需要获取当前的日期来显示正确的日期和时间。获取当前日期还可以用于数据记录和预约系统，以确保正确地记录事件发生的时间。

# 如何获取当前日期

在C编程中，获取当前日期需要使用time.h头文件中的函数。首先，我们需要定义一个time_t类型的变量来存储当前日期和时间。然后，使用time()函数来获取当前日期和时间，并将其存储在变量中。

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t current_time;
  time(&current_time);

  printf("当前日期和时间：%s", ctime(&current_time));
  
  return 0;
}
```
输出：
```
当前日期和时间：Tue Aug 10 09:47:19 2021
```

我们也可以使用tm结构体来获取更详细的日期信息，如年、月、日、小时、分钟和秒。

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t current_time;
  time(&current_time);

  struct tm *info;
  info = localtime(&current_time);

  printf("当前日期：%02d/%02d/%d\n", info->tm_mon + 1, info->tm_mday, info->tm_year + 1900);
  printf("当前时间：%02d:%02d:%02d\n", info->tm_hour, info->tm_min, info->tm_sec);

  return 0;
}
```
输出：
```
当前日期：08/10/2021
当前时间：09:51:28
```

# 深入了解获取当前日期

在C编程中，时间和日期都是以秒为单位表示的。time()函数返回自从当前时间（通常是1970年1月1日午夜）经过的秒数。然后，tm结构体可以使用这些秒数来计算当前日期和时间。

另外，需要注意的是，由于不同的操作系统使用不同的时间格式，所以在使用ctime()和localtime()函数时，需要考虑跨平台兼容性。

## 相关链接

- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C标准库：time.h](https://zh.cppreference.com/w/c/chrono)
- [深入理解C时间和日期函数](https://www.geeksforgeeks.org/time-functions-in-c-c/)