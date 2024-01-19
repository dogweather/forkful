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

## 什么 & 为什么?

计算未来或过去的日期是通过特定算法在当前日期的基础上增加或减少特定的日、月或年的时间。程序员会做这个主要是为了进行时间敏感的计算，比如任务调度、事件跟踪或者会话管理等。

## 如何操作:

以下是如何用C语言获取一个日期X天后的日期的简单示例:

```C 
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date = {0};
    time_t future;
    
    date.tm_year = 2021 - 1900;
    date.tm_mon = 8;  
    date.tm_mday = 1;
    
    future = mktime(&date) + (10 * 24 * 60 * 60);  //增加十天
    
    printf("未来的日期是:%s", asctime(localtime(&future)));
    
    return 0;
}
```
运行以上代码后，输出结果将会是:

```
未来的日期是:Wed Sep 11 00:00:00 2021
```
## 深入探索

计算未来或过去的日期这一概念自计算机诞生以来就存在，主要用于操作系统中任务调度、数据库中的时间戳记录等多种用途。在C语言中，我们使用time.h头文件中的函数来对时间进行操作。同时，也可以使用其它语言的相应函数或库来进行日期计算，例如Python的datetime库、Java的Calendar类等。

根据你的需要，你可以选择用日、月或者年为单位来进行计算。除了使用C标准库中的mktime以外，你还可以使用其它方式来进行日期计算，例如使用`strftime()`函数来格式化时间，使用`strptime()`函数来解析时间字符串等。

## 另请参阅

* C语言time.h库的[官方文档](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
* Python的datetime库的[官方文档](https://docs.python.org/3/library/datetime.html)
* Java的Calendar类的[官方文档](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Calendar.html)