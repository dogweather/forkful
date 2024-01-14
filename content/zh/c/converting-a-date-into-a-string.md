---
title:                "C: 将日期转换为字符串"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将日期转换为字符串？

在编程中，经常会遇到将日期转换为字符串的需求。这样做的好处是可以将日期以可读性高的方式展示给用户，让用户更容易理解和解读。在本文中，我们将通过实例展示如何使用C语言将日期转换为字符串，帮助读者更好地掌握这一过程。

## 如何实现日期转换为字符串？

首先，我们需要使用C语言中的时间函数`ctime()`来获取当前日期时间。然后，通过使用字符串格式化函数`strftime()`，我们可以将日期时间转换为可读性高的字符串格式。下面是一个简单的例子：

```C
#include <stdio.h>
#include <time.h>

int main() {

  time_t now;
  struct tm *local;
  char date_string[50];

  time(&now); // 获取当前日期时间
  local = localtime(&now); // 转换成本地时间格式
  strftime(date_string, 50, "%B %d, %Y %H:%M:%S", local); // 将日期时间格式化为字符串
  printf("当前日期时间为：%s", date_string);

  return 0;
}
```

运行以上代码会得到类似于`October 10, 2021 12:30:00`的字符串。我们还可以根据需要自行定义字符串的格式，如`%d/%m/%Y`将会显示为`10/10/2021`。在实际应用中，我们也可以通过用户输入的方式来获取日期，并将其转换为字符串格式。

## 深入了解日期转换为字符串

在深入研究日期转换为字符串的过程中，我们需要了解C语言中的时间相关函数。除了`ctime()`和`strftime()`，还有一些其他的函数可用于获取和操作日期时间，如`localtime()`、`mktime()`、`difftime()`等。此外，我们也可以通过改变系统的时区和日期格式，来影响日期转换的结果。有了这些知识，我们就可以更灵活地进行日期转换并处理相关问题。

## 参考链接

- [C语言时间函数介绍](https://www.runoob.com/cprogramming/c-date-time.html)
- [C语言字符串格式化函数介绍](https://www.runoob.com/cprogramming/c-function-strftime.html)
- [C语言时间处理常用函数总结](https://cwf529.github.io/2018/05/17/C++/Common-time-manipulation-functions-in-C++/)
- [C语言日期转换实例](https://codetanblog.com/2016/11/20/C%E8%AF%AD%E8%A8%80%E4%BB%A3%E7%A0%81%E5%AE%9E%E4%BE%8B-%E6%97%B6%E9%97%B4%E8%BD%AC%E6%8D%A2/)
- [如何通过C语言获取用户输入的日期并转换为字符串](https://www.geeksforgeeks.org/convert-struct-tm-to-string-in-c-language/) 

## 参见

- [时间和日期转换相关知识](https://www.w3schools.com/cpp/cpp_date.asp)
- [C语言中的时间函数文档](https://www.gnu.org/software/libc/manual/html_node/Calendar-Time-Functions.html)
- [各平台上的日期格式化指令](https://docs.microsoft.com/en-us/cpp/c-runtime-library/format-specification-syntax-printf-and-wprintf-functions?view=msvc-160)