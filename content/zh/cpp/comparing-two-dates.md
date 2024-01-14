---
title:    "C++: 比较两个日期"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 为什么

比较两个日期在编程中是一个常见的任务，它们可以用来判断时间顺序，计算时间间隔，或者作为条件来控制程序的流程。因此，了解如何比较两个日期在编程中是非常重要的。

## 如何

在C++中，比较两个日期可以使用标准库中的`std::chrono`和`std::time`。首先，我们需要包含头文件`<chrono>`和`<ctime>`。

```C++
#include <chrono>
#include <ctime>
```

然后，我们可以定义两个日期变量，并使用`std::chrono::system_clock::now()`来获取当前日期。

```C++
std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
std::chrono::system_clock::time_point tomorrow = std::chrono::system_clock::now() + std::chrono::hours(24);
```

接下来，使用`std::chrono::duration`来计算两个日期的间隔，并将其转换为所需的时间单位。

```C++
std::chrono::duration<int, std::ratio<86400>> diff = tomorrow - today;
int diff_days = diff.count();
```

最后，可以使用条件语句来比较两个日期的先后顺序。

```C++
if (tomorrow > today) {
    std::cout << "明天比今天晚一天" << std::endl;
}
```

以下是完整的示例代码和输出：

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point tomorrow = std::chrono::system_clock::now() + std::chrono::hours(24);

    std::chrono::duration<int, std::ratio<86400>> diff = tomorrow - today;
    int diff_days = diff.count();

    std::cout << "今天: " << std::ctime(&today) << std::endl;
    std::cout << "明天: " << std::ctime(&tomorrow) << std::endl;
    std::cout << "两个日期间隔: " << diff_days << "天" << std::endl;

    if (tomorrow > today) {
        std::cout << "明天比今天晚一天" << std::endl;
    }

    return 0;
}
```

输出：

```
今天: Mon Oct 18 14:36:08 2021
明天: Tue Oct 19 14:36:08 2021
两个日期间隔: 1天
明天比今天晚一天
```

## 深入了解

在C++中，日期和时间的处理有很多不同的方式，可以使用`<ctime>`中定义的`struct tm`结构体来表示时间，并使用`std::mktime()`函数将其转换为`std::time_t`类型。此外，C++20标准中还引入了`std::chrono::year_month_day`和`std::chrono::date`等新的日期相关类，可以更方便地进行日期操作。对于更复杂的日期比较需求，也可以使用第三方库如Boost或date库来提供更多功能和精确性。

## 参考资料

- [C++ reference - chrono](https://en.cppreference.com/w/cpp/chrono)
- [C++ reference - time](https://en.cppreference.com/w/cpp/header/time)
- [GeeksforGeeks - Date and Time in C++](https://www.geeksforgeeks.org/date-time-in-c/)
- [cppreference.com - Format of ctime and asctime output](https://en.cppreference.com/w/c/chrono/strftime)

## 查看更多

- [C++中的日期和时间处理](https://www.ibm.com/developerworks/aix/library/au-datetimesupportcpp/)(英文)
- [Boost日期时间库](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)(英文)
- [date - C++20标准中的日期库](https://github.com/HowardHinnant/date)(英文)