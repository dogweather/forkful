---
title:                "获取当天日期"
html_title:           "C++: 获取当天日期"
simple_title:         "获取当天日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting Current Date in C++

## What & Why?

获取当前日期是指在程序中获取当前日期信息的一种操作。程序员经常使用这个功能来跟踪时间，记录事件或者在输出中添加时间戳。这对于调试和日志记录非常有用。

## How to:

示例代码：

```C++
#include <iostream>
#include <ctime>

int main()
{
  // 获取当前时间信息并保存到time信息结构体中
  time_t now = time(0);

  // 将当前时间转换为字符串形式
  char* current_date = ctime(&now);

  // 输出当前日期
  std::cout << "当前日期为：" << current_date;
  
  return 0;
}
```

输出：

```
当前日期为：Sun Aug 15 12:56:23 2021
```

## Deep Dive

### Historical Context:

在早期，计算机系统并没有内置获取当前日期的功能。程序员通常会使用系统的时钟来计算时间，或者手动输入日期信息。随着操作系统的发展，获取当前日期的功能也被相应地集成进去。

### Alternatives:

除了使用ctime函数外，C++标准库还提供了其他获取日期信息的函数，比如strftime和localtime。此外，也可以使用第三方库来实现获取当前日期的功能。

### Implementation Details:

ctime函数返回的是一个指向静态内部字符串的指针。因此，在多次调用ctime函数时，返回的日期信息会是相同的。为了解决这个问题，可以使用localtime函数将time信息转换为本地时间，再进一步处理。

## See Also

- [ctime函数](https://www.cplusplus.com/reference/ctime/ctime/)
- [strftime函数](https://www.cplusplus.com/reference/ctime/strftime/)
- [localtime函数](https://www.cplusplus.com/reference/ctime/localtime/)