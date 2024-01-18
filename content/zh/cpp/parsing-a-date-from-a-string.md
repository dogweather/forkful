---
title:                "从字符串中解析日期"
html_title:           "C++: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 这是什么？为什么要做这个？

将日期从字符串中提取出来是指从字符串中提取出表示日期的部分。程序员经常这样做是为了将日期数据转换为计算机可以处理的形式，例如创建日历应用程序或进行日期比较等。

## 如何做？

下面是一个用C++编写的例子，演示如何将日期从字符串中提取出来。假设我们有一个字符串“2020年10月31日”，我们想从中提取出年份、月份和日期，可以按照以下步骤进行操作：

```C++
// 包含所需的标准库
#include <iostream>
#include <string>
#include <sstream>

using namespace std;

int main() {
    // 定义一个字符串
    string dateStr = "2020年10月31日";

    // 创建一个字符串流对象
    istringstream ss(dateStr);

    // 定义变量存储提取出来的日期
    int year, month, day;

    // 使用流对象从字符串中提取数值，并存储到变量中
    ss >> year >> month >> day;

    // 打印提取出来的日期
    cout << year << "年" << month << "月" << day << "日" << endl;

    return 0;
}
```

运行这段代码，输出将会是：2020年10月31日。

## 更多细节

提取日期这一概念并不是什么新鲜事，它可以追溯到早期的计算机编程，甚至更早。在过去，人们使用像FORTRAN这样的语言来处理日期，而今天，我们可以使用更现代的语言来更方便地提取日期，如C++。此外，还有其他方法可以提取日期，如使用正则表达式或特定的日期库。

## 参考链接

如果您想了解更多有关如何从字符串中提取日期的知识，可以参考以下链接：

- [关于C++处理日期的基础知识](https://www.ibm.com/support/pages/how-c-process-data-and-time-strings-and-structures)
- [使用C++的日期类来处理日期数据](https://developer.ibm.com/articles/learn-cpp-date-time-classes/)
- [了解正则表达式的使用方式](https://www.regular-expressions.info/)