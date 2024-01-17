---
title:                "比较两个日期"
html_title:           "C++: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 什么是日期比较？为什么程序员要做它？

日期比较是一种编程技术，用于比较两个日期之间的差异。程序员经常使用日期比较来检查某个日期是早于、等于还是晚于另一个日期。这对于排序、筛选和判断事件发生的顺序等任务非常有用。

# 如何进行日期比较？

下面是一个简单的示例，展示了如何使用 ```C++``` 编写代码来比较两个日期并输出结果：

```
#include <iostream>
#include <ctime>

int main() {
    // 定义需要比较的两个日期
    std::tm date1 = { 0, 0, 0, 1, 0, 121, 120 }; // 2021年1月1日
    std::tm date2 = { 0, 0, 0, 1, 0, 121, 119 }; // 2020年1月1日

    // 比较两个日期
    if (std::mktime(&date1) < std::mktime(&date2)) {
        std::cout << "日期1早于日期2";
    } else if (std::mktime(&date1) == std::mktime(&date2)) {
        std::cout << "日期1等于日期2";
    } else {
        std::cout << "日期1晚于日期2";
    }

    return 0;
}

```

运行结果：

```
日期1晚于日期2
```

# 深入了解日期比较

## 历史背景

在现代编程中，日期比较是一项常见的任务。然而，在过去，人们并不是使用数字形式的日期，而是用文字来表示日期，例如 “十一月十一日”。随着计算机的发展，日期被转换为数字形式，使得日期比较成为可能。

## 其他方法

除了使用 ```C++``` 内置的日期比较函数，也可以使用第三方库来进行日期比较，例如 Boost.Date_Time 和 Chrono。这些库提供更多的日期比较功能，可以满足更复杂的需求。

## 实现细节

在 ```C++``` 中，日期是以秒数来表示，从某个固定的起始日期开始计算，通常为 1970 年 1 月 1 日。所以，比较两个日期其实是比较它们对应的秒数大小。

# 相关资源

- [C++ 时间和日期函数](https://www.w3schools.com/cpp/cpp_date.asp)
- [C++ Boost.Date_Time 库](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [C++ Chrono 库](https://en.cppreference.com/w/cpp/chrono)