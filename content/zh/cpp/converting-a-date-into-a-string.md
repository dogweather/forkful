---
title:                "将日期转换为字符串"
html_title:           "C++: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
有时候，我们需要将日期（比如生日、纪念日等）转换成字符串的形式，比如在文件名中使用，或者显示在用户界面中。在本文中，我们将学习如何使用C++编程语言将日期转换成字符串，并且探索一些深层知识。

## 如何做
我们先来看一个简单的示例，将`2020-11-11`这个日期转换成字符串格式的`November 11, 2020`。

```C++
#include <iostream>
#include <iomanip>
#include <sstream>
using namespace std;

int main() {
    // 定义日期变量
    int year = 2020;
    int month = 11;
    int day = 11;

    // 使用stringstream来构建字符串
    stringstream ss;

    // 设置日期格式
    ss << setfill('0') << setw(2) << month << "/";
    ss << setfill('0') << setw(2) << day << "/";
    ss << year;

    // 将字符串打印出来
    cout << "转换结果：" << ss.str() << endl;

    return 0;
}
```
输出结果为：
```
转换结果：11/11/2020
```
现在让我们来看一下上面代码的解释：
- 首先，我们使用`iostream`和`sstream`库来处理输入和输出。
- 然后，定义了三个整型变量来存储日期的年、月、日。
- 接着，我们使用`stringstream`来构建一个字符串，它允许我们将多个字符串组合成一个字符串。
- 使用`setfill`函数来指定字符串中的空格填充字符为`0`，`setw`函数来指定字符串的宽度为`2`，然后使用`<<`运算符来将月份、日期和年份添加到`stringstream`中。
- 最后，使用`ss.str()`函数来获取字符串的值并输出到控制台。

除了这种简单的格式，我们也可以根据需要来定义更复杂的日期格式。C++提供了`strftime()`函数来帮助我们格式化日期，它使用一个格式字符串作为参数，并返回格式化后的日期字符串。下面是一个例子，将当前日期和时间转换成格式为`Monday, August 10, 2020 at 12:30:45 PM`的字符串：

```C++
#include <iostream>
#include <ctime> // 包含了时间相关的函数
using namespace std;

int main() {
    // 获取当前时间
    time_t now = time(0);

    // 将当前时间转换成字符串
    char* str_time = ctime(&now);

    // 输出转换结果
    cout << "转换结果：" << str_time << endl;

    return 0;
}
```
输出结果为：
```
转换结果：Mon Sep 30 13:54:50 2019
```

## 深入探讨
除了上面提到的两种方法，C++还有一些其它方法来将日期转换成字符串。一些常用的库比如`boost`和`Qt`也都提供了日期转换的功能。如果你需要使用这些库，你可以通过查阅它们的官方文档来了解具体的实现方法。

此外，在C++11标准中，也提供了一个`chrono`库来处理时间和日期相关的操作。它提供了一些新的类和函数，可以更方便地处理日期转换，比如`to_string()`函数来将日期转换成字符串，还有一些`duration`类来处理时间差值。如果你使用的是C++11及以上版本，你可以尝试使用这些新的特性来完成日期的转换。

## 参考链接
- [C++日期和时间转换](https://www.programiz.com/cpp-programming/time-date)
- [C++ strftime()函数](https://www.cplusplus.com/reference/ctime/strftime/)
- [C++11 chrono库](https://www.techiedelight.com/convert-string-to-int-stdstoi