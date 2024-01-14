---
title:    "C++: 获取当前日期"
keywords: ["C++"]
---

{{< edit_this_page >}}

【为什么：探讨C ++获取当前日期的重要性】

获取当前日期是编写程序时经常需要做的一个任务。它可以帮助我们跟踪时间、调试问题和创建日志。同时，它也是一种良好的编程习惯，在构建可靠的应用程序时必不可少。在本文中，我们将学习如何在C ++中获取当前日期，并深入探讨它的工作原理。

【如何实现：示例代码和输出】

为了在C++中获取当前日期，我们需要使用标准库中的日期和时间函数。首先，我们需要在程序开头包含 “```C++ #include <ctime> ```” 来引用这个库。接下来，我们可以使用 “```C++ time_t now = time(0); ```” 来获取当前日期的数值表示。最后，我们可以使用 “```C++ char* dt = ctime(&now); ```” 来将日期转换为字符串格式，并将其打印出来。

以下是完整的示例代码和输出：

```C++
#include <iostream>
#include <ctime>

int main()
{
    // 获取当前时间的数值表示
    time_t now = time(0);
    
    // 将日期转换为字符串格式
    char* dt = ctime(&now);
    
    // 打印输出
    std::cout << "当前日期为：" << dt << std::endl;
    
    return 0;
}
```

输出：

```
当前日期为：Mon Mar 26 10:23:17 2018
```

【深入探讨：如何获取当前日期的原理】

在我们的示例代码中，“time(0)” 返回一个数值，代表自 1970 年 1 月 1 日 00:00:00（协调世界时）以来的秒数。这个数值被称为“时间戳”。然后，我们将时间戳转换为一个字符串，这个字符串包含日期、时间和其他信息。注意，这个日期和时间是以当前系统时区为准的。因此，如果你需要使用不同的时区，可以使用 “```C++ struct tm *local = localtime(&now); ```” 函数来转换本地时间。

【相关阅读：学习更多有关日期和时间的信息】

- [C++ 参考手册：time 函数](https://zh.cppreference.com/w/cpp/chrono/c/time)
- [使用不同的时区来获取当前日期和时间](https://stackoverflow.com/questions/23019756/how-can-get-current-datetime-in-c-using-mktime-modified-such-current-time-disp)
- [学习如何处理日期和时间格式](https://www.programiz.com/cpp-programming/library-function/ctime/ctime)