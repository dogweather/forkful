---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？

从字符串解析日期就是从字符串形式的日期信息中获取年、月、日数据。程序员需要执行它以便于进行日期相关的计算和操作。

## 如是：

在C++中，你可以利用`std::get_time`与`std::istringstream`来实现这个过程。

代码示例：

```C++
#include<iostream>
#include<sstream>
#include<iomanip>
#include<ctime>

int main(){
    std::string dateStr = "2021-09-13";
    std::tm date = {};
    std::stringstream ss(dateStr);

    ss >> std::get_time(&date, "%Y-%m-%d");
    if(ss.fail()){
        std::cout << "Parsing failed\n";
    }else{
        std::cout << "Year: " << date.tm_year + 1900 << '\n'; //Note: tm_year is years from 1900.
        std::cout << "Month: " << date.tm_mon + 1 << '\n'; //Note: tm_mon is months from January (0-11).
        std::cout << "Day: " << date.tm_mday << '\n';
    }

    return 0;
}
```

输出：

```C++
Year: 2021
Month: 09
Day: 13
```

## 深入探讨

自从C++11开始，提供了基于时间的类型和函数，以便于日期和时间的操作。

在日期解析的领域，有很多库和方法也可以胜任这项任务，例如Boost库、QDate等。然而，`std::get_time`的好处在于它是标准库的一部分，避免了额外的依赖。

在`std::get_time`中，我们使用了格式化字符串（如`"%Y-%m-%d"`），这样可以灵活定义日期和时间的格式。

## 查看更多

- [C++官方文档—<ctime>](http://www.cplusplus.com/reference/ctime/)
- [GCC标准库文档—std::get_time](https://gcc.gnu.org/onlinedocs/libstdc++/libstdc++-html-USERS-4.3/a02089.html)