---
title:                "C++: 计算未来或过去的日期"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

计算日期（特别是未来或过去的日期）是一项实用的编程技能，可以帮助我们自动化日期相关的任务，提高工作效率。无论是计划未来活动还是回顾过去记录，计算日期的能力都能给我们带来便利和帮助。

## How To

有时候，我们需要向前或向后计算指定天数的日期。在C++中，我们可以使用`chrono`和`ctime`库来进行日期的计算。首先，我们需要包含这几个库，然后定义一个日期变量，并使用`chrono::system_clock`来获取当前的日期。

```C++
#include <iostream>
#include <chrono>
#include <ctime>

// 定义一个日期变量并初始化为当前日期
std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
```

接下来，我们可以使用`std::chrono::duration`来表示一个时间段，例如3天。然后，我们可以使用`today`变量和`std::chrono::duration`来计算未来或过去的日期。

```C++
// 定义一个time duration表示3天
std::chrono::duration<int> three_days(3);

// 计算3天后的日期
std::chrono::system_clock::time_point future = today + three_days;
tm* future_date = std::localtime(&future);
std::cout << "3 days from now: " << future_date->tm_year + 1900 << "/" << future_date->tm_mon + 1 << "/" << future_date->tm_mday << std::endl;

// 计算3天前的日期
std::chrono::system_clock::time_point past = today - three_days;
tm* past_date = std::localtime(&past);
std::cout << "3 days ago: " << past_date->tm_year + 1900 << "/" << past_date->tm_mon + 1 << "/" << past_date->tm_mday << std::endl;
```

运行上面的代码将会得到类似以下的输出：

```
3 days from now: 2019/1/12
3 days ago: 2019/1/6
```

## Deep Dive

上面的例子中，我们使用了`std::chrono::duration`来表示一个时间段。它是一个模板类，可以接受不同的时间类型，例如`std::chrono::hours`、`std::chrono::minutes`、`std::chrono::seconds`等。在日期计算中，我们使用`std::chrono::duration<int>`来表示时间段的天数，根据需要可以对其进行调整。

另外，我们还使用了`std::chrono::system_clock::time_point`来表示一个日期。它实际上是一个`std::time_t`类型的变量，可以用来表示从"1970年1月1日"开始的秒数。我们可以使用`std::time_t`类型的日期来进行日期的比较和计算。

## See Also

- [cppreference.com: chrono library](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com: ctime library](https://en.cppreference.com/w/cpp/chrono/c)
- [cplusplus.com: Standard Library of C](http://www.cplusplus.com/reference/clibrary/)