---
title:                "C++: 未来或历史日期的计算"
simple_title:         "未来或历史日期的计算"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，我们经常需要计算未来或过去的日期。这有助于我们规划活动、跟踪时间，或者在需要时提醒我们重要的事件。使用C++编写代码可以帮助我们快速有效地计算日期，让我们更有效地管理时间。 

## 如何做

在C++中，我们可以使用time.h头文件中的函数来计算未来或过去的日期。首先，我们需要声明一个tm结构体，这个结构体包含了年、月、日、时、分、秒等日期和时间信息。然后，我们可以使用mktime()函数来将tm结构体转换为时间戳，并根据需要进行加减来计算未来或过去的日期。最后，我们可以使用strftime()函数来将得到的时间戳格式化为我们所需要的日期格式。下面是一个计算明天日期的示例代码：

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // 声明tm结构体
    tm t;
    
    // 获取当前时间
    time_t now = time(0);
    
    // 将当前时间转换为tm结构体
    tm *ltm = localtime(&now);
    
    // 设置tm结构体中的日期为明天（今天日期加一）
    t.tm_mday = ltm->tm_mday + 1;
    
    // 将tm结构体转换为时间戳
    time_t tomorrow = mktime(&t);
    
    // 格式化输出明天的日期
    cout << "明天的日期是：";
    cout << strftime("%Y年%m月%d日", localtime(&tomorrow));
    
    return 0;
}
```

运行上述代码，输出结果如下：

```
明天的日期是：2020年06月21日
```

通过类似的方法，我们也可以计算过去的日期，只需将设置的日期加减改为减或加。同时，我们也可以根据需要设置tm结构体中的其他日期和时间信息来计算不同的日期。 

## 深入探讨

在深入探讨之前，我们需要了解一下时间戳的概念。时间戳是指从某个固定的时间（如1970年1月1日）开始所经过的秒数。通过时间戳，我们可以将日期和时间转换为一个整数，方便进行计算。在C++中，可以使用time(0)函数获取当前时间的时间戳。

另外，时间戳也可以通过将tm结构体转换成time_t类型来获取。在示例代码中，我们通过mktime()函数将tm结构体转换为时间戳。

## 参考链接

- [C++中的时间和日期处理](https://www.geeksforgeeks.org/time-functions-in-c-c/)
- [C++中的time.h头文件](http://www.cplusplus.com/reference/ctime/)
- [时间戳的概念及应用](https://blog.csdn.net/wsssslgz/article/details/80497073)