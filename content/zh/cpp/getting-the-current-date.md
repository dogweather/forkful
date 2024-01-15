---
title:                "获取当前日期"
html_title:           "C++: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在编程过程中，获取当前日期是很常见的需求。无论是记录日志、生成唯一的文件名，还是进行时间的计算，获取当前日期都是必不可少的步骤。

## 如何做

使用C++可以方便地获取当前日期。我们可以通过调用`time`函数来获取当前的时间戳，并使用`localtime`函数将其转换为本地日期和时间。下面是一个简单的示例代码，可以获取当前日期并打印出来。

```C++
#include <iostream>
#include <ctime>

int main() {
    // 获取当前时间戳
    time_t now = time(0);

    // 转换为本地日期和时间
    tm *ltm = localtime(&now);

    // 打印出当前日期
    std::cout << "当前日期：" << ltm->tm_year + 1900 << "/" 
              << ltm->tm_mon + 1 << "/" << ltm->tm_mday << std::endl;
    return 0;
}
```

运行以上代码，输出结果类似于：
```
当前日期：2021/4/11
```

## 深入了解

在C++中，`time_t`类型表示系统时间的秒数，从1970年1月1日0时0分0秒开始计算。而`tm`结构体则保存了本地日期和时间的一些信息。在调用`localtime`函数时，会将时间戳转换为本地时区的日期和时间，并保存在`tm`结构体中。在上面的代码中，我们通过`ltm->tm_year`、`ltm->tm_mon`和`ltm->tm_mday`来访问年、月和日的数值，需要注意的是，`tm`结构体中的月份和日期的数值都是从0开始的，所以需要分别加上1900和1才能得到正确的日期。

如果需要获取更详细的时间信息，例如小时、分钟和秒等，我们还可以通过访问`ltm`结构体中的其他成员来实现。具体的细节可以查阅C++官方文档。

## 参考链接

- [C++ time函数](https://www.cplusplus.com/reference/ctime/time/)
- [C++ localtime函数](https://www.cplusplus.com/reference/ctime/localtime/)

## 参见

- [C++中的日期和时间处理](https://www.runoob.com/cplusplus/cpp-date-time.html)
- [C++官方文档](https://devdocs.io/cpp/)