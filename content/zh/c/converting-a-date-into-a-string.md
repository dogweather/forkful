---
title:                "将日期转换为字符串"
html_title:           "C: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##为什么？

为什么要把日期转换成字符串？这个问题可能会让你感觉有点无聊，但是实际上，这是一个在编程中非常常见的任务。无论是要在输出中显示日期，还是要将日期存储到数据库中，都需要将其转换为字符串格式。

##怎么做？

首先，你需要知道C语言中的日期是如何表示的。通常，日期被存储为一个长整型变量，表示从某个固定时间点（通常是1970年1月1日）开始经过的秒数。要将其转换为字符串，我们需要使用内置的 "sprintf()" 函数。

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 获取当前时间
    time_t now = time(NULL);

    // 将时间转换为字符串
    char time_str[30];
    sprintf(time_str, "%s", ctime(&now));

    // 输出结果
    printf("当前时间为：%s\n", time_str);
    return 0;
}
```

运行上面的代码，你会得到类似这样的输出：

```
当前时间为：Mon Jul 12 23:05:26 2021
```

##深入探讨

如果你想要进一步探索日期转换为字符串的细节，你可以了解一下 "strftime()" 函数。它可以让你自定义日期的格式，而不是使用默认的字符串格式。你也可以阅读一些相关的库，如 <time.h> 和 <stdlib.h>，它们提供了更多的日期转换工具和函数。

##另请参阅

- [Sprintf函数手册](https://docs.microsoft.com/zh-cn/cpp/c-runtime-library/reference/sprintf-s-sprintf-l-sprintf-snprintf-l-snprintf-s)
- [Time.h函数手册](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C标准库教程](https://www.runoob.com/cprogramming/c-standard-library.html)