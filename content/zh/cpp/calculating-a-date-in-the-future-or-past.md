---
title:                "计算未来或过去的日期"
html_title:           "C++: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么和为什么？ (What & Why?)

计算未来或过去的日期是指定定一个日期，并据此增加或减少一定数量的天数、月份或年份。程序员之所以这样做，是因为在很多应用中，特别是和预定、计划或记录有关的应用，需要进行日期计算。

## 如何实现：(How to:)
以下是一个简单的C++程序，使用 `std::chrono`库来计算未来的日期：

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std;
    using namespace chrono;

    system_clock::time_point today = system_clock::now(); // 获取今天的日期

    time_point<system_clock, duration<int, ratio_multiply<hours::period, ratio<24>>>> tomorrow = today + duration<int, ratio_multiply<hours::period, ratio<24>>>(1); // 获取明天的日期

    time_t tt = system_clock::to_time_t(tomorrow);

    cout << "Tomorrow will be: " << ctime(&tt);

    return 0;
}
```

程序输出可能是：

``` 
Tomorrow will be: Tue Jan  8 15:51:44 2022
```

## 深入研究 (Deep Dive)
1). 历史背景:在早期的编程语言中，计算日期偏移量通常更为复杂，并且在跨年、跨月等情况下需要手动处理许多细节。而现代语言（例如C++11及其后版本）提供的库函数能自动处理这些细节。

2). 替代选项:除了 `std::chrono`之外，Boost日期时间库（Date_Time）也是一个强大的计算日期的选项。

3). 实现细节:`std::chrono`库以一种通用且安全的方式表示时间，可以在不同的时间单位之间进行转换，并且可以安全地进行日期运算，防止溢出或精度丢失。

## 另请参阅 (See Also)
1). C++参考: std::chrono: <https://en.cppreference.com/w/cpp/chrono>
2). Boost Date_Time: <https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html>
3). ISO C++委员会:关于日期时间库的最新工作： <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p0355r7.html>