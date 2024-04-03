---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:40.398851-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728\u73B0\u4EE3C++\u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528`<chrono>`\u5E93\u6765\u672C\u5730\u5904\u7406\u65E5\u671F\u548C\
  \u65F6\u95F4\uFF0C\u4F46\u5B83\u4E0D\u76F4\u63A5\u652F\u6301\u4ECE\u5B57\u7B26\u4E32\
  \u89E3\u6790\uFF0C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u683C\u5F0F\u9700\u8981\u624B\
  \u52A8\u89E3\u6790\u3002\u7136\u800C\uFF0C\u5BF9\u4E8EISO 8601\u65E5\u671F\u683C\
  \u5F0F\u548C\u7B80\u5355\u7684\u81EA\u5B9A\u4E49\u683C\u5F0F\uFF0C\u4E0B\u9762\u662F\
  \u4F60\u53EF\u4EE5\u5B9E\u73B0\u89E3\u6790\u7684\u65B9\u6CD5\u3002 **\u4F7F\u7528\
  `<chrono>`\u548C`<sstream>`\uFF1A**."
lastmod: '2024-03-13T22:44:48.120814-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u73B0\u4EE3C++\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`<chrono>`\u5E93\
  \u6765\u672C\u5730\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\uFF0C\u4F46\u5B83\u4E0D\
  \u76F4\u63A5\u652F\u6301\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\uFF0C\u5BF9\u4E8E\u66F4\
  \u590D\u6742\u7684\u683C\u5F0F\u9700\u8981\u624B\u52A8\u89E3\u6790\u3002\u7136\u800C\
  \uFF0C\u5BF9\u4E8EISO 8601\u65E5\u671F\u683C\u5F0F\u548C\u7B80\u5355\u7684\u81EA\
  \u5B9A\u4E49\u683C\u5F0F\uFF0C\u4E0B\u9762\u662F\u4F60\u53EF\u4EE5\u5B9E\u73B0\u89E3\
  \u6790\u7684\u65B9\u6CD5."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

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
