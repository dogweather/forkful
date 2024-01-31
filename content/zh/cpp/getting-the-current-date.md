---
title:                "获取当前日期"
date:                  2024-01-20T15:13:23.870695-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

category:             "C++"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

获取当前日期意味着在程序中获得当前的年、月、日信息。程序员这么做通常是为了记录事件发生的时间或者对时间敏感的操作，例如生成报告、用户日志或是算出日期差。

## How to: (如何操作：)

```C++
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(nullptr);   // 获取当前时间
    std::tm* now = std::localtime(&t);    // 转换为本地时间

    std::cout << "Year: " << (now->tm_year + 1900) << '\n'   // 输出年份
              << "Month: " << (now->tm_mon + 1) << '\n'      // 输出月份
              << "Day: " << now->tm_mday << std::endl;       // 输出日期

    return 0;
}
```

运行结果：

```
Year: 2023
Month: 3
Day: 14
```

## Deep Dive (深入探讨：)

获取日期并不是新的需求，在早期，C语言就为此提供了 `<ctime>` 库。经过多年的发展，C++ 引入了更多现代和易用的库，如 `<chrono>`。`<ctime>` 实际上是 C++ 对 C 语言标准库 `<time.h>` 的适配，它提供了一套围绕时间的函数。

除了 `<ctime>`，C++11 引入了 `<chrono>` 库，提供了稳定、类型安全的时间点和持续时间的操作。即便 `<ctime>` 有效，但 `<chrono>` 库通常是更现代和偏好的方式。

实现细节上，`time_t` 是 C++ 用来表示时间的基础类型，它通常是一个长整形，表示自 1970-01-01 00:00:00 UTC 起的秒数。通过调用 `localtime` 函数，我们可以将 `time_t` 类型的值转换为表示本地时间的 `tm` 结构。

## See Also (另请参阅：)

- C++ `<chrono>` 库的使用：https://en.cppreference.com/w/cpp/header/chrono
- C++ `<ctime>` 库的详细介绍：https://en.cppreference.com/w/cpp/header/ctime
- 更多时间日期处理相关的推荐实践和例子：https://github.com/HowardHinnant/date
