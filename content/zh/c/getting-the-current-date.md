---
title:                "C: 获取当前日期"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么: 获取当前日期的重要性

在编写程序时，经常需要获取当前日期作为参考，以便执行特定的操作。比如在开发日记应用程序时，需要自动记录当前日期的日志，或者在开发预约系统时，需要获取当前日期以便进行预约安排。获取当前日期可以让程序具有更强大的功能，提升用户体验。

## 如何: 使用C语言获取当前日期

在C语言中，有一个特别的函数可以帮助我们获取当前日期，它就是 `time()` 函数。首先，我们需要包含 `<time.h>` 头文件，接着调用 `time()` 函数，将返回的时间戳（以秒为单位）赋值给 `time_t` 类型的变量。然后，使用 `localtime()` 函数将时间戳转换为本地时间，并将结果赋值给 `struct tm` 类型的变量。最后，可以通过使用该结构体变量的成员来获取当前日期的年、月、日等信息。下面是一个简单的示例代码：

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    struct tm *local_time;

    // 获取当前时间戳
    current_time = time(NULL);

    // 转换为本地时间
    local_time = localtime(&current_time);

    // 打印当前日期
    printf("%d年%d月%d日\n", (local_time->tm_year + 1900), (local_time->tm_mon + 1), local_time->tm_mday);

    return 0;
}
```

上述代码中，`localtime()` 函数会自动转换为本地时间，因此无需考虑时区的问题。最后的输出结果可能为：

```
2021年6月15日
```

## 深入了解: 获取当前日期的原理

在上面的示例代码中，我们调用了 `time()` 和 `localtime()` 两个函数来获取当前日期。那么这两个函数是如何工作的呢？其实， `time()` 函数会返回一个表示当前时间的时间戳，该时间戳是从1970年1月1日零点开始计算的秒数。而 `localtime()` 函数则将时间戳转换为本地时间，并返回一个 `struct tm` 结构体变量，其中包含了当前日期的年、月、日、时、分、秒等信息。

另外，需要注意的是， `time()` 函数的返回值是一个 `time_t` 类型的变量，其实质上也是一个长整型数，可以使用 `long` 或 `long long` 类型的变量来接收。

## 参考资料

- [C语言获取当前日期的方法](https://www.runoob.com/cprogramming/c-standard-library-time-h.html)
- [C语言标准库中的时间函数](https://zh.cppreference.com/w/c/chrono)

## 参见

- [C语言教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [Markdown语法指南](https://www.runoob.com/markdown/md-tutorial.html)