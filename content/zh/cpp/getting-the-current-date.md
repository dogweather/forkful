---
title:    "C++: 获取当前日期"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么要获取当前日期？

在编写程序时，经常需要获取当前的日期。这可以帮助我们跟踪时间，比如在日志记录中添加时间戳，或者在需要定期执行某些任务时判断日期。在C++中，获取当前日期是一项基本的操作，而且非常方便和灵活。

## 如何获取当前日期？

在C++中，获取当前日期需要使用标准库中的`<ctime>`头文件。下面是一个简单的例子，展示如何获取当前日期并输出它：

```C++
#include <iostream>
#include <ctime>

int main() {
    // 获取当前日期
    time_t now = time(0);
    
    // 转换为可读格式
    char* date = ctime(&now);
    
    // 输出当前日期
    std::cout << "当前日期为：" << date << std::endl;
    
    return 0;
}
```

运行以上代码，输出将会是类似于以下格式的当前日期：

```bash
当前日期为：Mon Jun 21 11:54:59 2021
```

## 深入了解获取当前日期

在上面的例子中，我们使用了`ctime()`函数来获取当前日期。它的作用是将一个时间值转换为一个可读的C字符串。这个时间值通常是从`time()`函数中获取的，它返回一个表示自Epoch（计算机系统中某一刻开始的时间）以来的秒数的`time_t`类型的值。`ctime()`函数接受一个指向这个`time_t`值的指针，然后将它转换为一个字符串表示。

除了`ctime()`函数外，C++标准库中还有其他有用的函数来获取当前日期，比如`localtime()`和`strftime()`。你可以尝试使用它们来获取当前日期的不同格式。同时，也可以阅读关于`<ctime>`头文件的更多详细信息来深入了解如何操作日期和时间。

## 另请参阅

- [C++ Reference: <ctime>](https://www.cplusplus.com/reference/ctime/)
- [GeeksforGeeks: C++ Standard Template Library <ctime>](https://www.geeksforgeeks.org/cpp-standard-template-library-stl/)
- [C++ Plus: Date and Time in C++](https://www.cplusplus.com/forum/articles/36725/)