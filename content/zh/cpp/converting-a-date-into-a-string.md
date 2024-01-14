---
title:    "C++: 将日期转换为字符串"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
转换日期为字符串是一项非常常见的编程任务，适用于许多不同的场景。例如，当您需要在日志中记录时间戳时，或者需要将日期显示在用户界面上，转换日期为字符串都是必需的。

## 如何
要将日期转换为字符串，您可以使用strftime函数。该函数需要两个参数，第一个参数是格式字符串，用于指定要将日期转换成的格式，第二个参数是您想要转换的日期值。例如，假设我们有一个名为“today”的日期变量，它存储的是今天的日期。我们想要将它转换为“May 23, 2021”的字符串格式。我们可以使用以下代码来实现：

```C++
#include <iostream> 
#include <ctime>

int main() 
{ 
    time_t today = time(0); // 获取当前日期
    char str[50]; // 用于存储转换后的字符串
    strftime(str, 50, "%B %d, %Y", localtime(&today)); // 转换日期为字符串
    std::cout << str; // 输出结果：May 23, 2021
    
    return 0; 
}
```

## 深入探讨
在上面的示例中，我们使用了“%B %d, %Y”格式字符串来指定转换后的日期格式。您可以根据自己的需要选择不同的格式字符串来转换日期。下面是一些常用的格式代码：

- %B - 月份的全称
- %b - 月份的缩写
- %d - 日期，带前导零
- %m - 月份，带前导零
- %Y - 年份，4位数
- %y - 年份，2位数
- %H - 小时，24小时制
- %I - 小时，12小时制
- %M - 分钟
- %S - 秒

要了解更多关于strftime函数的详细信息，您可以查看C++的官方文档。

## 参考资料
- [strftime函数文档](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [C++ 日期和时间](https://www.cplusplus.com/reference/ctime/)
- [C++ 中文学习站](https://zh.cppreference.com/w/%E9%A6%96%E9%A1%B5)