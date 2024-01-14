---
title:    "C: "
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么比较两个日期
当涉及日期和时间的编程时，我们经常需要比较两个日期。比如，你可能想要确定哪个日期在未来，或者计算两个日期之间的时间差。比较日期可以帮助我们更好地处理日期和时间数据，使程序更加准确和有效。

## 如何比较两个日期
在C语言中，我们可以使用`time.h`头文件中的`difftime()`函数来比较两个日期。下面是一个简单的例子：

```C
#include <stdio.h>
#include <time.h>

int main(void) {
    // 定义两个日期
    struct tm date1 = { 0 };
    struct tm date2 = { 0 };

    // 设置日期的值
    date1.tm_year = 2019 - 1900;
    date1.tm_mon = 5 - 1;
    date1.tm_mday = 1;

    date2.tm_year = 2019 - 1900;
    date2.tm_mon = 5 - 1;
    date2.tm_mday = 5;

    // 计算日期差值
    double diff = difftime(mktime(&date2), mktime(&date1));

    // 打印结果
    printf("Date difference: %.0f days", diff / (60 * 60 * 24));

    return 0;
}
```

运行结果：
```
Date difference: 4 days
```

## 深入了解比较两个日期
在深入了解比较两个日期之前，首先需要了解日期和时间在计算机中是如何表示的。通常，在C语言中，我们使用`time.h`头文件中的`struct tm`结构来表示日期和时间。该结构包含年、月、日、时、分、秒、星期几等信息。我们可以通过`mktime()`函数将`struct tm`结构转换为`time_t`类型的数值，然后使用`difftime()`函数来比较两个日期。

需要注意的是，在C语言中，日期和时间的起点是1970年1月1日 00:00:00 UTC，也被称为UNIX纪元。因此，比较日期时，我们需要确保使用正确的起点。另外，还有一些其他的日期和时间函数，如`strftime()`和`gmtime()`，可以帮助我们更方便地处理时间数据。

# 参考链接
- [C语言中的日期和时间](https://www.programiz.com/c-programming/c-date-time)
- [时间操作函数参考](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C语言：比较两个日期之间的天数](https://stackoverflow.com/questions/13106322/c-comparing-days-between-two-times-how-does-it-actually-work)

# 参见
- [C语言中日期和时间的格式化](https://github.com/bjddd192/MD-DOS/blob/master/Tips/003.md)