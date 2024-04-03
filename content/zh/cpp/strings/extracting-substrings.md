---
date: 2024-01-20 17:45:10.024042-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u6307\u4ECE\u4E00\u4E2A\u5B57\
  \u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5B57\u7B26\u5E8F\u5217\u7684\u8FC7\u7A0B\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5206\u6790\u3001\u5904\
  \u7406\u7279\u5B9A\u6570\u636E\u6216\u7B80\u5316\u6587\u5B57\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.098071-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u662F\u6307\u4ECE\u4E00\u4E2A\u5B57\
  \u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5B57\u7B26\u5E8F\u5217\u7684\u8FC7\u7A0B\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5206\u6790\u3001\u5904\
  \u7406\u7279\u5B9A\u6570\u636E\u6216\u7B80\u5316\u6587\u5B57\u64CD\u4F5C\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to (如何操作)


### Example 1: 使用 `substr`
```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World!";
    std::string subString = fullString.substr(7, 5); // 从位置7开始提取5个字符
    std::cout << subString << std::endl; // 输出: World
    return 0;
}
```

### Example 2: 使用 `string::iterator`
```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World!";
    std::string subString(fullString.begin() + 7, fullString.begin() + 12);
    std::cout << subString << std::endl; // 输出: World
    return 0;
}
```

## Deep Dive (深入探讨)
在C++中，字符串处理是常见任务。`std::string` 是 C++ 标准库的一部分，从 C++98 开始存在，提供了强大的字符串操作功能。

`substr` 功能从 C++98 开始就存在了，它是最直接的提取子字符串方法。在 C++11 中，`std::string` 得到了增强，包括范围构造函数和迭代器，为处理字符串提供更多的灵活性。

除了 `substr` 和迭代器，还有其他方式来提取字符串，如使用 `std::istringstream` 类，正则表达式(`std::regex`)等。

实现细节上，`substr` 通常会创建一个新的字符串副本，这可能影响性能。在处理大量数据时，选择正确的实现方法非常关键。

## See Also (另请参阅)
- C++ std::string 类文档: https://cplusplus.com/reference/string/string/
- C++ string handling 总览: https://cppreference.com/w/cpp/string/basic_string
- C++ 正则表达式库: https://cplusplus.com/reference/regex/
