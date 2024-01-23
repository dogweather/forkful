---
title:                "获取当前日期"
date:                  2024-01-20T15:13:06.991286-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

获取当前日期在编程中就是读取系统的现在的日期。程序员这么做是因为许多应用需要使用时间戳，比如记录日志、数据有效性验证或者界面显示。

## How to: (如何操作：)

在C语言中，你可以用`<time.h>`库获取当前日期。下面示例展示了如何做到这一点：

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;

    time(&rawtime); // 获取原始时间
    timeinfo = localtime(&rawtime); // 转换为本地时间结构

    printf("当前日期和时间: %s", asctime(timeinfo)); // 显示时间

    return 0;
}
```

这段代码运行后，输出会是这样（输出会随实际日期变化）：
```
当前日期和时间: Fri Mar 10 21:46:07 2023
```

## Deep Dive (深度了解)

获取当前日期和时间在早期计算机系统中就很重要了。`<time.h>`是C语言标准库中的一部分，提供了多种处理时间和日期的函数。

除了 `localtime` 和 `asctime`，其他函数比如 `gmtime`（获取世界时间）和 `strftime` （格式化日期和时间）也可用来获取和处理时间。实现细节方面，时间通常自1970年1月1日（Unix纪元）开始的秒数来计算。

如果需要更高精度的时间或者不同的时间格式，C11标准引入了 `<chrono.h>`，但是`<time.h>`在大多数系统中仍然充分。

## See Also (另见)

- C标准库文档 (`<time.h>`): [https://en.cppreference.com/w/c/chrono](https://en.cppreference.com/w/c/chrono)
- 关于时间处理的在线教程: [https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
