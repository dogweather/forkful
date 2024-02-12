---
title:                "获取当前日期"
date:                  2024-02-03T19:09:18.073668-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
在 C++ 中获取当前日期是一个基本任务，对于需要基于系统时钟处理或显示日期的程序来说至关重要。它对于日志记录、时间戳记、任务调度以及任何依赖于日期和时间的功能都是必不可少的。

## 如何实现：
C++ 提供了几种获取当前日期的方法，包括 C++ 标准库和第三方库，如 Boost。以下示例展示了如何完成这个任务。

### 使用 `<chrono>`（C++20 及以后版本）
C++20 在 `<chrono>` 库中引入了更多功能，使得获取当前日期变得直截了当：
```cpp
#include <iostream>
#include <chrono>
#include <format> // 对于 std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // 捕获当前时间
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // 转换为 time_t

    // 将时间格式化为可读格式
    std::cout << "当前日期: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**示例输出：**
```plaintext
当前日期: 2023-03-15
```

### 使用 `<ctime>`
对于使用旧版本 C++ 或偏好传统 C 库的程序员：
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // 获取当前时间
    std::tm* now = std::localtime(&t);
    std::cout << "当前日期: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**示例输出：**
```plaintext
当前日期: 2023-03-15
```

### 使用 Boost Date_Time
对于使用 Boost 库的项目，Boost Date_Time 库提供了一个获取当前日期的替代方法：
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // 使用 Boost 的格里历日历获取当前日
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "当前日期: " << today << std::endl;

    return 0;
}
```
**示例输出：**
```plaintext
当前日期: 2023-Mar-15
```
这些示例为使用 C++ 处理日期提供了基本基础，对广泛的应用程序至关重要。
