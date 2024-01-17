---
title:                "未来或过去日期的计算"
html_title:           "C++: 未来或过去日期的计算"
simple_title:         "未来或过去日期的计算"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么是计算未来或过去日期？为什么程序员会这么做？

计算未来或过去日期是通过编程来确定一个指定日期之前或之后的日期。程序员通常会这么做是因为处理日期和时间数据在许多应用程序中都是很重要的。例如，在预订航班票时，需要计算未来的日期来确认最佳的出发时间。 

## 如何进行计算？

下面是一个示例代码，用于计算七天之后的日期，并打印输出结果： 

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    tm currentDate = {};
    // 获取当前日期
    time_t now = time(0);
    tm* ptr = localtime(&now);
    // 设置日期为当前日期，如果想要计算过去日期，可以将数字加上负号
    currentDate.tm_mday = ptr->tm_mday;
    currentDate.tm_mon = ptr->tm_mon;
    currentDate.tm_year = ptr->tm_year + 1900;
    // 添加七天
    currentDate.tm_mday += 7;
    // 进行日期转换
    mktime(&currentDate);
    // 打印输出结果
    cout << "Seven days from now will be: " << asctime(&currentDate);
    return 0;
}
```

## 深入探讨

- 历史背景：计算日期的想法可以追溯到公历制定之前，人们就已经开始使用天文学来测算时间。现在，我们使用的计算机系统是基于西方历法，因此在编程中，也采用了相同的方法来计算日期。
- 其他方法：除了使用```mktime```函数来计算日期，还可以使用其他库和算法来实现，例如C++标准库中的```chrono```库。
- 实现细节：上面示例代码中使用了```tm```结构体来存储日期和时间，需要注意结构体的成员变量的命名规则和范围限制。

## 参考资料

- [C++ chrono库](https://www.cplusplus.com/reference/chrono/)
- [时间和日期计算的历史](https://www.wikiwand.com/en/Astronomy_and_calendars_in_ancient_and_medieval_China)