---
title:                "C: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

##为什么

计算机编程是一个充满创造性的过程，在现代社会中有着广泛的应用。作为一种主流编程语言，C语言广泛运用于系统开发、网络编程和嵌入式设备中。获取当前日期是一个常见的需求，它可以帮助我们记录和跟踪系统运行的日期，也可以用来实现各种不同的功能。在本文中，我将介绍如何在C语言中获取当前日期，并深入解析其实现原理。

##如何做到

以下示例代码将演示如何在C语言中使用time.h头文件中的函数来获取当前日期。

```C
include <stdio.h>
include <time.h>

int main() {
    // 调用time函数，获取当前时间的秒数
    time_t now = time(NULL);
    // 将秒数转换为日期结构体
    struct tm *current_time = localtime(&now);
    // 使用结构体中的成员变量，分别获取年、月、日
    int year = current_time->tm_year + 1900;
    int month = current_time->tm_mon + 1;
    int day = current_time->tm_mday;
    // 打印输出当前日期
    printf("当前日期是：%d年%d月%d日\n", year, month, day);
    return 0;
}
```

该代码的输出如下所示：

```
当前日期是：2020年8月12日
```

可以看到，我们成功获取到了当前的日期，并打印输出了相应的信息。下面我们来深入了解一下这段代码的实现原理。

##深入解析

在C语言中，获取当前日期的功能是由time.h头文件中的函数来实现的。主要使用的函数有time和localtime。time函数是标准库函数，它会返回当前时间的秒数。而localtime函数则会将这个秒数转换为一个日期结构体，结构体中包含了年、月、日等信息。需要注意的是，结构体中的年变量是从1900年开始计算，月份是从0开始计算，因此需要进行对应的调整。最后，我们使用printf函数将获取到的日期信息打印输出。

除了time和localtime函数外，time.h头文件中还有其他一些函数可以帮助我们获取更详细的日期信息，比如获取小时、分钟、秒等。感兴趣的读者可以自行探索。

##参考阅读

如果你想进一步了解关于日期和时间的函数，可以参考以下链接：

- [C语言中日期和时间的函数](https://www.runoob.com/cprogramming/c-date-time.html)
- [关于time.h头文件的详细解释](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/time.htm)
- [深入了解C语言的时间和日期处理](https://github.com/iamslash/C/blob/master/B%c3%a1sic/Time/README.md)

##请参见

- [Markdown语法指南](https://markdown.cn/)
- [C语言基础教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [学习C语言的好处](https://zhuanlan.zhihu.com/p/38662595)