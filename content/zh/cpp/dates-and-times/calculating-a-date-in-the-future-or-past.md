---
title:                "计算未来或过去的日期"
aliases: - /zh/cpp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:34.530304-07:00
model:                 gpt-4-1106-preview
simple_title:         "计算未来或过去的日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期是确定距今一定天数的日期。程序员这么做是为了处理预定事件、记录期限或者自动化任务日程。

## 如何做：
```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
    using namespace std::chrono;

    //当前日期
    system_clock::time_point today = system_clock::now();
    //转换为tm结构
    time_t tt = system_clock::to_time_t(today);
    tm local_tm = *localtime(&tt);
  
    //打印今天的日期
    std::cout << "今天是: " << std::put_time(&local_tm, "%Y-%m-%d") << std::endl;

    //加上10天
    system_clock::time_point future_date = today + days(10);
    tt = system_clock::to_time_t(future_date);
    local_tm = *localtime(&tt);
  
    //打印未来的日期
    std::cout << "十天后是: " << std::put_time(&local_tm, "%Y-%m-%d") << std::endl;
  
    //减去30天
    system_clock::time_point past_date = today - days(30);
    tt = system_clock::to_time_t(past_date);
    local_tm = *localtime(&tt);
  
    //打印过去的日期
    std::cout << "三十天前是: " << std::put_time(&local_tm, "%Y-%m-%d") << std::endl;

    return 0;
}
```

## 深入探索
日期的计算有很长的历史。在早期，日期运算常常是用标准库函数如`time`和`mktime`手动实现。但现在，C++提供了`<chrono>`库，方便且准确地处理日期和时间。

不同编程语言有不同的库来处理时间。例如，Python有`datetime`模块，Java有`java.time`包。在C++中，`<chrono>`库以一种类型安全的方式提供了时间点和持续时间的概念。

为何选择`<chrono>`库？它在类型间的转换处理上提供了高级别的错误检查，这让编程时减少了bug的可能性，并且简化了代码。此外，`<chrono>`库允许你以不同的时间单位来进行操作，增加了灵活性。

## 另请参阅
- 《C++标准库》by Nicolai M. Josuttis: https://www.cppstdlib.com/
- C++ reference - Chrono library: https://en.cppreference.com/w/cpp/chrono
- ISO C++ - Date & Time utilities: https://isocpp.org/std/the-standard

确保查看这些资源来加深理解，并且获取关于C++中日期与时间处理的更多细节。
