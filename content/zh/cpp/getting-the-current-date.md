---
title:                "C++: 了解目前日期"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获得当前日期的时间戳可以帮助您记录和跟踪程序运行时发生的事件，比如用户输入和输出数据。这在调试程序和记录日志时非常有用。

如何：在C++中，通过使用ctime标准库中的time函数，可以轻松地获取当前日期的时间戳。首先，需要包含<ctime>头文件。然后，使用`time( )`函数来获取当前时间的秒数，`localtime()`函数将其转换为本地时区的时间结构，然后再通过`mktime()`函数将时间结构转换为UTC时间。最后，使用`asctime()`函数将UTC时间转换为字符串格式的日期和时间。

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // 获取当前时间的秒数
    time_t now = time(0);

    // 将秒数转换为本地时间结构
    struct tm* localTime = localtime(&now);

    // 将本地时间结构转换为UTC时间
    time_t UTCTime = mktime(localTime);

    // 将UTC时间转换为字符串格式的日期和时间
    cout << "当前日期和时间为：" << asctime(localtime(&UTCTime)) << endl;

    return 0;
}

// 输出：
// 当前日期和时间为：Tue Jul 20 09:13:36 2021
```

深入了解：除了time函数，C++还提供了其他一些函数来获取当前日期和时间。例如，使用`strftime()`函数可以自定义日期和时间的输出格式，`difftime()`函数可以计算两个时间之间的差值。另外，通过引入第三方库，比如Boost库中的Date Time库，可以让日期和时间的操作更加简便和灵活。

## 请参阅
- https://www.cplusplus.com/reference/ctime/time/
- https://www.geeksforgeeks.org/c-program-print-current-day-date-time/
- https://stackoverflow.com/questions/997946/how-to-get-current-time-and-date-in-c/28848164#28848164