---
title:                "比较两个日期"
date:                  2024-01-20T17:32:15.317065-07:00
model:                 gpt-4-1106-preview
simple_title:         "比较两个日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
比较两个日期就是对比它们来看哪一个更早或者更晚。程序员通常这么做来处理事件顺序，有效期限和时间段。

## How to: (如何操作：)
在C中，比较日期通常借助`time.h`头文件中定义的`struct tm`。下面是一些实用的代码示例：

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = { .tm_year=120, .tm_mon=5, .tm_mday=15 }; // 2020-06-15
    struct tm date2 = { .tm_year=122, .tm_mon=5, .tm_mday=15 }; // 2022-06-15

    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    if (time1 < time2) {
        printf("Date1 is earlier than Date2.\n");
    } else if (time1 > time2) {
        printf("Date2 is earlier than Date1.\n");
    } else {
        printf("Date1 and Date2 are the same.\n");
    }

    return 0;
}
```

样例输出：
```
Date1 is earlier than Date2.
```

## Deep Dive (深入探讨)
比较日期并不是新鲜事，但随着计算机编程的进步，这一过程得以自动化。在C语言的早期版本中没有直接支持日期的类型，开发者需要手动处理。使用`struct tm`和`time_t`，这是C标准库提供的数据结构和类型，极大简化了日期比较。

除了上述方法，你还可以选择其他的库，比如`<chrono>`（C++11引入的）或时间处理库`<date.h>`。

日期比较的实现细节值得注意的是，`time_t`类型是用来表示时间的秒数（自1970-01-01起的格林威治时间），而结构体`tm`用于表示更直观的日期和时间。

## See Also (参考链接)
- C标准库文档：<https://en.cppreference.com/w/c/chrono>
- `<chrono>`入门：<https://en.cppreference.com/w/cpp/chrono>
- Howard Hinnant's Date library：<https://github.com/HowardHinnant/date>