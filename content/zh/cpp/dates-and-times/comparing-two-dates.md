---
date: 2024-01-20 17:32:39.270259-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\
  \u662F\u786E\u5B9A\u5B83\u4EEC\u7684\u5148\u540E\u5173\u7CFB\u3002\u7A0B\u5E8F\u5458\
  \u9700\u8981\u8FD9\u4E48\u505A\u6765\u5904\u7406\u65F6\u95F4\u903B\u8F91\uFF0C\u6BD4\
  \u5982\u8BA1\u7B97\u65F6\u5DEE\u3001\u9A8C\u8BC1\u6709\u6548\u671F\u4EE5\u53CA\u5B89\
  \u6392\u4E8B\u4EF6\u987A\u5E8F\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.691406-07:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\
  \u662F\u786E\u5B9A\u5B83\u4EEC\u7684\u5148\u540E\u5173\u7CFB\u3002\u7A0B\u5E8F\u5458\
  \u9700\u8981\u8FD9\u4E48\u505A\u6765\u5904\u7406\u65F6\u95F4\u903B\u8F91\uFF0C\u6BD4\
  \u5982\u8BA1\u7B97\u65F6\u5DEE\u3001\u9A8C\u8BC1\u6709\u6548\u671F\u4EE5\u53CA\u5B89\
  \u6392\u4E8B\u4EF6\u987A\u5E8F\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中，比较两个日期就是确定它们的先后关系。程序员需要这么做来处理时间逻辑，比如计算时差、验证有效期以及安排事件顺序。

## 如何做：
```C++
#include <iostream>
#include <ctime>

int main() {
    std::tm date1 = {};
    std::tm date2 = {};

    // 设置日期1为2023年4月1日
    date1.tm_year = 123; // 从1900年开始算
    date1.tm_mon = 3;    // 从0开始算（0-11）
    date1.tm_mday = 1;

    // 设置日期2为2023年4月2日
    date2.tm_year = 123;
    date2.tm_mon = 3;
    date2.tm_mday = 2;

    // 将tm结构转换为time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // 比较日期
    if (time1 < time2) {
        std::cout << "日期1早于日期2。" << std::endl;
    } else if (time1 > time2) {
        std::cout << "日期1晚于日期2。" << std::endl;
    } else {
        std::cout << "两个日期相同。" << std::endl;
    }

    return 0;
}
```

输出可能是：
```
日期1早于日期2。
```

## 深入探究
早期的C++版本并没有专门的日期时间库，通常使用C语言的 `<ctime>` 处理日期时间。C++11引入了 `<chrono>` 库，提供了更好的时间点和持续时间管理。不过，对于日期的比较，最简单的方式仍然是使用C标准库的`tm`结构和`mktime`函数。这种方法适合通用场景，但如果需要更高精度或处理诸如时区和夏令时的复杂问题，就需要考虑用`<chrono>`库或第三方库了。

C++20进一步改善了日期和时间的处理机制，引入了新的标准库模块`<chrono>`，提供了对日期、时钟和时区的全面支持。

日常使用中，除了时间戳比较，你还可能需要处理实际的日历日期。这时候，使用如boost.date_time等第三方库将提供更多灵活性和功能。

## 查看更多
- C++ `<chrono>`库文档: https://en.cppreference.com/w/cpp/header/chrono
- C标准库时间处理 `<ctime> `: https://en.cppreference.com/w/c/chrono
- Boost.Date_Time库官网: https://www.boost.org/doc/libs/release/libs/date_time/
