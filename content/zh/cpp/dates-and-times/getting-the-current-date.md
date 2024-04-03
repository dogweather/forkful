---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:18.073668-07:00
description: "\u5728 C++ \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\
  \u57FA\u672C\u4EFB\u52A1\uFF0C\u5BF9\u4E8E\u9700\u8981\u57FA\u4E8E\u7CFB\u7EDF\u65F6\
  \u949F\u5904\u7406\u6216\u663E\u793A\u65E5\u671F\u7684\u7A0B\u5E8F\u6765\u8BF4\u81F3\
  \u5173\u91CD\u8981\u3002\u5B83\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\u5F55\u3001\u65F6\u95F4\
  \u6233\u8BB0\u3001\u4EFB\u52A1\u8C03\u5EA6\u4EE5\u53CA\u4EFB\u4F55\u4F9D\u8D56\u4E8E\
  \u65E5\u671F\u548C\u65F6\u95F4\u7684\u529F\u80FD\u90FD\u662F\u5FC5\u4E0D\u53EF\u5C11\
  \u7684\u3002"
lastmod: '2024-03-13T22:44:48.122077-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C++ \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\u4E2A\u57FA\
  \u672C\u4EFB\u52A1\uFF0C\u5BF9\u4E8E\u9700\u8981\u57FA\u4E8E\u7CFB\u7EDF\u65F6\u949F\
  \u5904\u7406\u6216\u663E\u793A\u65E5\u671F\u7684\u7A0B\u5E8F\u6765\u8BF4\u81F3\u5173\
  \u91CD\u8981\u3002\u5B83\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\u5F55\u3001\u65F6\u95F4\u6233\
  \u8BB0\u3001\u4EFB\u52A1\u8C03\u5EA6\u4EE5\u53CA\u4EFB\u4F55\u4F9D\u8D56\u4E8E\u65E5\
  \u671F\u548C\u65F6\u95F4\u7684\u529F\u80FD\u90FD\u662F\u5FC5\u4E0D\u53EF\u5C11\u7684\
  \u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
