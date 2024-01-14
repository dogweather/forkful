---
title:    "C: 获取当前日期"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

#为什么

在编程中，我们经常需要获取当前日期。无论是为了记录数据、生成文件名或者其他用途，获取当前日期是一项基本的任务。

#如何做

获取当前日期是C语言中的一项基本任务，我们可以通过使用标准库中的time.h头文件来轻松实现。下面是一个简单的例子，演示了如何使用C语言来获取当前日期并打印输出。

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 获取当前日期
    time_t currentDate = time(NULL);
    
    // 将当前日期格式化为字符串
    char* formattedDate = ctime(&currentDate);
    
    // 打印输出当前日期
    printf("当前日期为：%s\n", formattedDate);
    
    return 0;
}
```

输出：

```
当前日期为：Sun Jul 19 14:56:45 2020
```

以上代码中，我们使用time()函数来获取当前日期的时间戳，然后使用ctime()函数将时间戳转换为字符串格式，最后使用printf()函数来打印输出当前日期。值得注意的是，以上代码输出的日期格式依赖于操作系统和编译器，可能会有所不同。

#深入了解

除了上面提到的方法，我们还可以使用struct tm结构来获取更加具体和灵活的日期信息。首先，我们需要将time_t类型转换为struct tm结构，然后就可以通过结构中的各种成员来获取日期的详细信息，如年、月、日、时、分、秒等。

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // 获取当前日期的time_t类型
    time_t currentDate = time(NULL);
    
    // 将time_t类型转换为struct tm结构
    struct tm* dateInfo = localtime(&currentDate);
    
    // 打印输出当前日期的详细信息
    printf("当前日期为：%d年%d月%d日 %d:%d:%d\n", dateInfo->tm_year + 1900,
           dateInfo->tm_mon + 1, dateInfo->tm_mday, dateInfo->tm_hour,
           dateInfo->tm_min, dateInfo->tm_sec);
    
    return 0;
}
```

输出：

```
当前日期为：2020年7月19日 14:56:45
```

通过使用struct tm结构，我们可以更加精确地获取当前日期的各个成员的数值，从而实现更加定制化的需求。

#请参阅

- [C语言的time.h头文件](https://www.cplusplus.com/reference/ctime/)
- [time()函数](https://www.cplusplus.com/reference/ctime/time/)
- [ctime()函数](https://www.cplusplus.com/reference/ctime/ctime/)
- [struct tm结构](https://www.cplusplus.com/reference/ctime/tm/)