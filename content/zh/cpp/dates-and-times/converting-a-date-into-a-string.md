---
date: 2024-01-20 17:36:10.083586-07:00
description: "\u5728C++\u4E2D\uFF0C\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\
  \u6362\u610F\u5473\u7740\u4F60\u5C06\u65E5\u671F\u683C\u5F0F\u5316\u4E3A\u6587\u672C\
  \u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4EE5\u4FBF\u66F4\u5BB9\u6613\
  \u5730\u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7528\u6237\u754C\u9762\u4E2D\
  \u4F7F\u7528\u65E5\u671F\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.180593
model: gpt-4-1106-preview
summary: "\u5728C++\u4E2D\uFF0C\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\u6362\
  \u610F\u5473\u7740\u4F60\u5C06\u65E5\u671F\u683C\u5F0F\u5316\u4E3A\u6587\u672C\u5F62\
  \u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4EE5\u4FBF\u66F4\u5BB9\u6613\u5730\
  \u663E\u793A\u3001\u5B58\u50A8\u6216\u8005\u5728\u7528\u6237\u754C\u9762\u4E2D\u4F7F\
  \u7528\u65E5\u671F\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在C++中，日期到字符串的转换意味着你将日期格式化为文本形式。程序员这样做以便更容易地显示、存储或者在用户界面中使用日期。

## How to: (如何操作：)
```C++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

int main() {
    std::time_t t = std::time(nullptr);   // 获取当前时间
    std::tm* tm_ptr = std::localtime(&t); // 转换为本地时间

    // 创建一个输出字符串流
    std::ostringstream date_stream;

    // 使用strftime来格式化日期
    char buffer[80];
    std::strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", tm_ptr);
    
    // 将格式化的字符串输出到字符串流
    date_stream << buffer;

    // 从字符串流获取字符串
    std::string date_str = date_stream.str();

    std::cout << "Formatted Date String: " << date_str << std::endl;

    return 0;
}
```
输出样例:
```
Formatted Date String: 2023-03-15 12:45:30
```

## Deep Dive (深入探讨)
- 历史: C++11之前，日期和时间的处理通常需要自行实现或使用第三方库。C++标准库直到C++11引入了`<chrono>`库后，时间处理才变得简单可靠。
- 替代方案: `<chrono>`库提供了基于现代C++实践的时间处理功能。但是，至今仍然有程序员使用`<ctime>`库处理日期时间。
- 实现细节: 使用`std::ostringstream`可以灵活地创建需要的字符串格式。可依据需要使用`std::put_time(tm_ptr, "%Y-%m-%d %H:%M:%S")`代替`strftime`。

## See Also (参见)
- C++ `<chrono>` Library: https://en.cppreference.com/w/cpp/header/chrono
- Format specifiers for `strftime`: https://www.cplusplus.com/reference/ctime/strftime/
- String Streams in C++ (`std::ostringstream`): https://en.cppreference.com/w/cpp/io/basic_ostringstream
- Time Manipulation (`<ctime>` header): https://en.cppreference.com/w/cpp/header/ctime
