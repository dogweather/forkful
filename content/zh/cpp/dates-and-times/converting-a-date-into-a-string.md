---
date: 2024-01-20 17:36:10.083586-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) - \u5386\u53F2: C++11\u4E4B\
  \u524D\uFF0C\u65E5\u671F\u548C\u65F6\u95F4\u7684\u5904\u7406\u901A\u5E38\u9700\u8981\
  \u81EA\u884C\u5B9E\u73B0\u6216\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002C++\u6807\
  \u51C6\u5E93\u76F4\u5230C++11\u5F15\u5165\u4E86`<chrono>`\u5E93\u540E\uFF0C\u65F6\
  \u95F4\u5904\u7406\u624D\u53D8\u5F97\u7B80\u5355\u53EF\u9760\u3002 - \u66FF\u4EE3\
  \u65B9\u6848:\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.332251-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) - \u5386\u53F2: C++11\u4E4B\u524D\uFF0C\
  \u65E5\u671F\u548C\u65F6\u95F4\u7684\u5904\u7406\u901A\u5E38\u9700\u8981\u81EA\u884C\
  \u5B9E\u73B0\u6216\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002C++\u6807\u51C6\u5E93\
  \u76F4\u5230C++11\u5F15\u5165\u4E86`<chrono>`\u5E93\u540E\uFF0C\u65F6\u95F4\u5904\
  \u7406\u624D\u53D8\u5F97\u7B80\u5355\u53EF\u9760\u3002 - \u66FF\u4EE3\u65B9\u6848\
  : `<chrono>`\u5E93\u63D0\u4F9B\u4E86\u57FA\u4E8E\u73B0\u4EE3C++\u5B9E\u8DF5\u7684\
  \u65F6\u95F4\u5904\u7406\u529F\u80FD\u3002\u4F46\u662F\uFF0C\u81F3\u4ECA\u4ECD\u7136\
  \u6709\u7A0B\u5E8F\u5458\u4F7F\u7528`<ctime>`\u5E93\u5904\u7406\u65E5\u671F\u65F6\
  \u95F4\u3002 - \u5B9E\u73B0\u7EC6\u8282: \u4F7F\u7528`std::ostringstream`\u53EF\u4EE5\
  \u7075\u6D3B\u5730\u521B\u5EFA\u9700\u8981\u7684\u5B57\u7B26\u4E32\u683C\u5F0F\u3002\
  \u53EF\u4F9D\u636E\u9700\u8981\u4F7F\u7528`std::put_time(tm_ptr, \"%Y-%m-%d %H:%M:%S\"\
  )`\u4EE3\u66FF`strftime`\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

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
