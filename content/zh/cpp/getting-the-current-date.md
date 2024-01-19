---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# What & Why?
获取当前日期是一种获取计算机系统当前日期的编程操作，通常返回年、月、日的具体值。这对于日志记录、时间戳生成、计算时间间隔等功能十分重要。

# How to:
可以使用C++ `<chrono>` 和 `<ctime>`库来获取当前日期。下面是一个示例程序：

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main(){
    auto current = std::chrono::system_clock::now();
    std::time_t currentTime = std::chrono::system_clock::to_time_t(current);
    
    std::cout << "Current date: " << std::ctime(&currentTime);
    return 0;
}
```
运行后的输出将会是这样：
```Bash
Current date: Sat Jun 19 11:36:59 2021
```

# Deep Dive
历史上，C++不具备内置的日期或时间库，往往依赖于操作系统 API。C++11 引入了 `<chrono>` 库，提供了有关日期/时间的高级功能。

除了使用 `<chrono>`库外，旧的方式是直接使用 `<ctime>`库，提供了相应的日期和时间函数，如 `time(0)`.

获取当前日期操作的具体方式，取决于操作系统和C++编译器。并且返回的日期格式由操作系统区域设置决定。

# See Also
- [C++ <chrono> Library](http://www.cplusplus.com/reference/chrono/)
- [C++ <ctime> Library](http://www.cplusplus.com/reference/ctime/)