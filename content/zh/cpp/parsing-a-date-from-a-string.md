---
title:                "从字符串解析日期"
date:                  2024-01-20T15:35:16.616780-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
将字符串解析成日期，就是把文本格式转换为日期类型。程序员这么做是为了方便对日期进行处理，比如验证或计算时间差。

## How to: (如何做：)
用 `std::istringstream` 和 `std::get_time` 来解析日期。看代码：

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string date_str = "2023-03-15";
    std::tm tm = {};
    std::istringstream ss(date_str);

    ss >> std::get_time(&tm, "%Y-%m-%d");
    if (ss.fail()) {
        std::cout << "解析失败" << std::endl;
    } else {
        std::cout << "解析成功: " << std::put_time(&tm, "%Y-%m-%d") << std::endl;
    }

    return 0;
}

```
输出样例：
```
解析成功: 2023-03-15
```

## Deep Dive (深入了解)
历史上，C++程序员可能会使用 `strptime` 或手动解析日期。`std::get_time`和`std::put_time` C++11起成为标准库的一部分，简化了这个过程。除了`istringstream`，还可以用 `std::chrono` 和第三方库如`boost::date_time`。

实现时需要考虑的细节有日期格式的符号（比如 `%Y` 表示4位年份）和错误处理（如`ss.fail()`检测）。

## See Also (另请参阅)
- C++ `std::get_time` 官方文档: https://en.cppreference.com/w/cpp/io/manip/get_time
- C++ `std::put_time` 官方文档: https://en.cppreference.com/w/cpp/io/manip/put_time
- 关于 `std::chrono`: https://en.cppreference.com/w/cpp/chrono
- Boost.Date_Time库文档: https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html
