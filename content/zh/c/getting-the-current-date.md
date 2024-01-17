---
title:                "获取当前日期"
html_title:           "C: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是指在程序中获取系统当前日期的过程。程序员这样做的原因是为了方便记录和处理数据以及跟踪程序运行状态。

## 如何：
```C
#include <stdio.h>
#include <time.h>

int main() {
    // 获取当前日期
    time_t rawtime;
    struct tm * timeinfo;
    char buffer[80];
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    
    // 格式化输出
    strftime (buffer, 80, "当前日期：%Y-%m-%d", timeinfo);
    printf(buffer);
    return 0;
}
```

## 深入了解:
* 历史背景：在早期的计算机系统中，日期处理需要耗费大量的资源和时间。随着计算机技术的发展，获取当前日期变得更加简单高效。
* 替代方案：除了上述代码中使用的`time`库外，也可以使用`sys/time.h`库来获取当前日期。
* 实现细节：程序通过`timeinfo`结构体来存储日期信息，其中包含年、月、日等数据，然后使用`strftime`函数来格式化输出日期。

## 参考资料：
* [C程序语言参考手册](https://zh.wikipedia.org/wiki/%E5%BE%AE%E8%BD%AFMSVC)
* [C库参考手册](https://www.runoob.com/cprogramming/c-function-localtime.html)