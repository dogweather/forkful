---
date: 2024-01-20 17:28:34.530304-07:00
description: "\u5982\u4F55\u505A\uFF1A \u65E5\u671F\u7684\u8BA1\u7B97\u6709\u5F88\u957F\
  \u7684\u5386\u53F2\u3002\u5728\u65E9\u671F\uFF0C\u65E5\u671F\u8FD0\u7B97\u5E38\u5E38\
  \u662F\u7528\u6807\u51C6\u5E93\u51FD\u6570\u5982`time`\u548C`mktime`\u624B\u52A8\
  \u5B9E\u73B0\u3002\u4F46\u73B0\u5728\uFF0CC++\u63D0\u4F9B\u4E86`<chrono>`\u5E93\uFF0C\
  \u65B9\u4FBF\u4E14\u51C6\u786E\u5730\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u3002\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.418175-06:00'
model: gpt-4-1106-preview
summary: "\u4E0D\u540C\u7F16\u7A0B\u8BED\u8A00\u6709\u4E0D\u540C\u7684\u5E93\u6765\
  \u5904\u7406\u65F6\u95F4\u3002\u4F8B\u5982\uFF0CPython\u6709`datetime`\u6A21\u5757\
  \uFF0CJava\u6709`java.time`\u5305\u3002\u5728C++\u4E2D\uFF0C`<chrono>`\u5E93\u4EE5\
  \u4E00\u79CD\u7C7B\u578B\u5B89\u5168\u7684\u65B9\u5F0F\u63D0\u4F9B\u4E86\u65F6\u95F4\
  \u70B9\u548C\u6301\u7EED\u65F6\u95F4\u7684\u6982\u5FF5."
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
