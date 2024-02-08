---
title:                "从字符串解析日期"
aliases:
- zh/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:40.398851-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
