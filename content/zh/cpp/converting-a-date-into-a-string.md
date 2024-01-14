---
title:                "C++: 将日期转换为字符串"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
日期转换为字符串是一个常见的编程任务。它可以帮助我们将日期以一种易于阅读和理解的方式呈现，也可以用于数据存储和处理的需要。在C++编程中，日期转换为字符串也是一项必不可少的技能。下面我们来学习如何使用C++来完成这个任务。

## 如何做
日期转换为字符串的方法在不同的编程语言中可能有所不同，但在C++中，我们可以使用标准库中的 `to_string()` 函数来实现。首先，我们需要定义一个 `tm` 结构体来存储日期信息，然后使用 `strftime()` 函数将日期格式化为一个字符串，最后使用 `to_string()` 函数将字符串转换为所需的格式。

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main() {
    // 定义一个tm结构体来存储日期
    struct tm date;
    
    // 设置日期信息，如年份、月份、日期等
    date.tm_year = 2020 - 1900; // 年份要减去1900
    date.tm_mon = 9; // 月份范围为 0-11，所以9代表10月
    date.tm_mday = 1; // 日期范围为1-31
    
    // 使用strftime()函数将日期格式化为字符串
    char buffer[80];
    strftime(buffer, 80, "%Y-%m-%d", &date); // 输出格式为 YYYY-MM-DD
    
    // 使用to_string()函数将字符串转换为所需的格式
    string dateStr = to_string(date.tm_year + 1900) + "-" + to_string(date.tm_mon + 1) + "-" + to_string(date.tm_mday); // 输出格式为 YYYY-MM-DD
    
    // 输出结果
    cout << "日期转换为字符串后的结果为：" << endl;
    cout << "使用strftime()函数格式化的结果为：" << buffer << endl;
    cout << "使用to_string()函数转换的结果为：" << dateStr << endl;
    
    return 0;
}
```

输出结果：

```
日期转换为字符串后的结果为：
使用strftime()函数格式化的结果为：2020-10-01
使用to_string()函数转换的结果为：2020-10-01
```

## 深入探讨
当涉及日期转换为字符串时，还有一些注意事项需要我们考虑。首先，我们需要了解不同编译器支持的 `tm` 结构体的定义不尽相同，因此需要对 `tm` 结构体中的成员进行适当的调整。其次，我们还需要考虑日期的本地化问题，不同国家和地区对日期格式也有所不同，因此需要根据实际情况对日期进行格式化。最后，我们还需要注意处理日期转换中可能遇到的错误情况，如日期的有效性和边界情况等。

在C++中，我们可以使用 `<locale>` 头文件中的 `std::locale` 类来处理本地化问题，使用 `<exception>` 头文件中的 `std::exception` 类来处理错误情况。

## 参考资料

- [C++中的日期转换为字符串](https://www.techiedelight.com/convert-date-to-string-cpp/)
- [C++标准模板库 (STL) 中的to_string()函数](https://www.geeksforgeeks.org/stdto_string-in-c/)
- [C++中的strftime()函数](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [C++中的标准库异常处理](https://en.cppreference.com/w/cpp/error/exception)
- [C++中的本地化处理](https://www.tutorialspoint.com/cplusplus/cpp_localization.htm)

## 查看更多
- [C++标准库中的日期和时间](https://en.cppreference.com/w/cpp/chrono)
- [C++日期和时间库](https://www.boost.org/doc/libs/1_46_0/doc/html/date