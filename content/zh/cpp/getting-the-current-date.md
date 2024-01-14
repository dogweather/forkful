---
title:                "C++: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么
当前日期是计算机程序中一个非常常见的功能。通过获取当前日期，可以在程序中动态地显示日期，或者用于记录数据的时间戳。这个功能在许多不同的领域都会用到，所以掌握如何获取当前日期是很重要的。

## 如何
要在C++中获取当前日期，可以使用标准库中的`<ctime>`头文件里的`time()`函数。首先需要包含头文件，然后调用函数并储存返回的`time_t`数据类型。以下是一个简单的示例代码：

```C++
#include <iostream>
#include <ctime>

int main()
{
    // 获取当前时间
    time_t now = time(0);
    
    // 将当前时间转换为字符串
    char* currentDate = ctime(&now);
    
    // 输出当前日期和时间
    std::cout << "当前日期和时间：" << currentDate << std::endl;
    
    return 0;
}
```

输出结果会根据当前时间不同而变化，例如：

```
当前日期和时间：Mon Nov 09 13:45:12 2020
```

## 深入了解
`time()`函数返回的是当前时间距离标准时间（1970年1月1日00:00:00）的秒数。这个秒数可以用来计算其他时间单位，例如天、小时、分钟等等。另外，`ctime()`函数将`time_t`数据类型转换为一个C风格字符串，这也是为什么上面的示例代码中需要用`char*`来储存结果。

除了`time()`和`ctime()`，还有其他可以获取当前日期的函数，例如`localtime()`和`strftime()`。每个函数都有自己的优势和特点，可以根据实际需求选择最适合的函数来使用。

## 参考资料
- [C++ Reference: time()](https://en.cppreference.com/w/cpp/chrono/c/time)
- [C++ Reference: ctime()](https://en.cppreference.com/w/cpp/chrono/c/ctime)
- [C++ Reference: localtime()](https://en.cppreference.com/w/cpp/chrono/c/localtime)
- [C++ Reference: strftime()](https://en.cppreference.com/w/cpp/chrono/c/strftime)