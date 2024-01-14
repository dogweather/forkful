---
title:                "C: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

有时候，在编程的过程中，我们需要将日期转换成字符串。这可能是因为我们需要将日期显示给用户，或者将它保存到文件中。无论何种原因，日期转换成字符串是一项常见的任务，而且对于学习C语言的初学者来说也是一项重要的技能。

## 如何

下面是一个简单的示例，演示如何使用C语言将日期转换成字符串：

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 获取当前日期和时间
    time_t now = time(NULL);

    // 将日期转换为字符串
    char date_string[30];
    strftime(date_string, 30, "%Y-%m-%d %H:%M:%S", localtime(&now));

    // 打印字符串
    printf("当前日期时间为：%s\n", date_string);

    return 0;
}
```

输出结果可能是这样的：

```
当前日期时间为：2021-12-01 16:30:00
```

让我们来解析一下上面的代码。首先，我们使用C标准库中的`time()`函数来获取当前日期和时间。然后，使用`strftime()`函数将日期转换为字符串，并指定了日期格式为`%Y-%m-%d %H:%M:%S`，即年-月-日 时:分:秒。最后，使用`printf()`函数将字符串打印出来。

除了上面的例子，我们还可以通过改变日期格式来获得不同的输出。例如，如果我们想要将日期转换成指定语言的字符串，可以使用`setlocale()`函数来设置语言环境，并使用对应语言的日期格式。

## 深入了解

在C语言中，日期实际上是以一个整数来表示的，即从1970年1月1日开始经过的秒数。因此，将日期转换成字符串实际上是将一个整数值格式化成一定的形式。

在C语言中，还有一个重要的日期库，即`<ctime>`。它提供了许多用于处理日期和时间的函数，例如计算时间差、比较日期、检查闰年等等。如果想要深入了解日期与时间在C语言中的处理，可以进一步学习这个库。

## 参考链接

- [C标准库文档](https://www.cplusplus.com/reference/)
- [C语言实例教程：日期和时间](https://www.runoob.com/cprogramming/c-examples-date.html)
- [如何学习C语言](https://www.zhihu.com/question/19803556)

## 参见

- [日期和时间格式化指令](https://www.cplusplus.com/reference/ctime/strftime/)