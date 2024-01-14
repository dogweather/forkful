---
title:    "C: 将日期转换为字符串。"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将日期转换为字符串？

日期是我们日常生活中经常需要处理的数据。将日期转换为字符串可以使得我们更容易地处理、存储和显示日期数据。在C语言中，有多种方法可以将日期转换成字符串，让我们一起来看看吧！

## 如何将日期转换为字符串

在C语言中，我们可以使用`strftime()`函数来将日期格式化为字符串。以下是一个简单的例子，展示了如何使用`strftime()`函数来将当前日期转换为字符串并输出到屏幕上：

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 获取当前日期和时间
    time_t current_time = time(NULL);
    // 将日期格式化为字符串
    char date_str[20];
    strftime(date_str, 20, "%Y-%m-%d", localtime(&current_time));
    // 输出日期字符串
    printf("今天的日期是：%s", date_str);
    return 0;
}
```

运行结果为：

```
今天的日期是：2020-10-30
```

使用`strftime()`函数时，需要定义一个字符数组作为日期字符串的缓冲区，并且指定日期格式。上面例子中使用的日期格式`%Y-%m-%d`代表年份、月份和日期。除了这个日期格式外，还有其他很多可用的日期格式，可以根据需求选择使用。

## 深入了解日期转换为字符串

在C语言中，日期和时间是用`time_t`类型来表示的，这个类型是一个整型数，表示自某个特定时间以来经过的秒数。而`strftime()`函数可以将`time_t`类型的数据格式化为人类友好的日期字符串。

另外，值得注意的是，`strftime()`函数还可以接受`struct tm`类型的数据，这个类型是一个包含日期和时间信息的结构体。我们也可以使用`strptime()`函数将日期字符串转换为`struct tm`类型的数据。这种方式更灵活，可以让我们自由地选择想要的日期格式。

## 参考链接

- [C语言中strptime()和strftime()函数的用法 - CSDN博客] (https://blog.csdn.net/mikicheng/article/details/81230375)
- [C日期和时间处理函数 - 菜鸟教程] (https://www.runoob.com/cprogramming/c-date-time.html)
- [C标准库函数 - strftime() - man页] (https://linux.die.net/man/3/strftime)