---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:40.398851-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u5305\u62EC\u89E3\u91CA\
  \u5B57\u7B26\u4E32\u683C\u5F0F\u4EE5\u63D0\u53D6\u65E5\u671F\u7EC4\u4EF6\uFF0C\u5982\
  \u65E5\u3001\u6708\u548C\u5E74\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u7528\u6237\u8F93\u5165\uFF0C\u8BFB\u53D6\u6570\u636E\u6587\u4EF6\
  \uFF0C\u6216\u4E0E\u4EE5\u5B57\u7B26\u4E32\u683C\u5F0F\u901A\u4FE1\u65E5\u671F\u7684\
  API\u4EA4\u4E92\u3002\u8FD9\u5BF9\u4E8E\u6570\u636E\u5904\u7406\u3001\u9A8C\u8BC1\
  \u4EE5\u53CA\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\u8FDB\u884C\u65E5\u671F\u7B97\u672F\
  \u975E\u5E38\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.120814-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u5305\u62EC\u89E3\u91CA\
  \u5B57\u7B26\u4E32\u683C\u5F0F\u4EE5\u63D0\u53D6\u65E5\u671F\u7EC4\u4EF6\uFF0C\u5982\
  \u65E5\u3001\u6708\u548C\u5E74\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5904\u7406\u7528\u6237\u8F93\u5165\uFF0C\u8BFB\u53D6\u6570\u636E\u6587\u4EF6\
  \uFF0C\u6216\u4E0E\u4EE5\u5B57\u7B26\u4E32\u683C\u5F0F\u901A\u4FE1\u65E5\u671F\u7684\
  API\u4EA4\u4E92\u3002\u8FD9\u5BF9\u4E8E\u6570\u636E\u5904\u7406\u3001\u9A8C\u8BC1\
  \u4EE5\u53CA\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\u8FDB\u884C\u65E5\u671F\u7B97\u672F\
  \u975E\u5E38\u91CD\u8981\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 什么 & 为什么？
从字符串解析日期包括解释字符串格式以提取日期组件，如日、月和年。程序员这样做是为了处理用户输入，读取数据文件，或与以字符串格式通信日期的API交互。这对于数据处理、验证以及在应用程序中进行日期算术非常重要。

## 如何做：
在现代C++中，你可以使用`<chrono>`库来本地处理日期和时间，但它不直接支持从字符串解析，对于更复杂的格式需要手动解析。然而，对于ISO 8601日期格式和简单的自定义格式，下面是你可以实现解析的方法。

**使用`<chrono>`和`<sstream>`：**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601格式
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "解析的日期: " << parsed_date << std::endl;
    } else {
        std::cout << "解析日期失败。" << std::endl;
    }
    
    return 0;
}
```
示例输出：
```
解析的日期: 2023-04-15
```

对于更复杂的格式或处理旧版C++版本时，像`date.h`（Howard Hinnant的日期库）这样的第三方库很受欢迎。以下是如何使用它解析各种格式：

**使用`date.h`库：**
确保你已经安装了库。你可以在[这里](https://github.com/HowardHinnant/date)找到它。

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "解析的日期: " << parsed_date << std::endl;
    } else {
        std::cout << "从字符串解析日期失败。" << std::endl;
    }

    return 0;
}
```
示例输出（可能根据您的系统区域设置和日期设置而有所不同）：
```
解析的日期: 2023-04-15
```
