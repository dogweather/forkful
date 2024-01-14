---
title:                "C++: 比较两个日期"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要比较两个日期时，例如检查用户的生日是否在未来某个日期，或者计算一段时间过去了多少天，我们就需要使用日期比较功能。

## 怎么做

首先，我们需要创建两个日期变量，表示我们要比较的两个日期。接下来，我们使用`<`（小于）或`>`（大于）运算符来比较这两个日期。如果我们想要比较的是相同日期，可以使用`==`（等于）运算符。

```C++
// 创建日期变量
int date1 = 20210516;
int date2 = 20210520;

// 使用运算符来比较日期
if (date1 < date2) {
    cout << "日期1在日期2之前" << endl;
} else if (date1 > date2) {
    cout << "日期1在日期2之后" << endl;
} else if (date1 == date2) {
    cout << "日期1和日期2相同" << endl;
}
```

运行上面的代码会得到输出：`日期1在日期2之前`。

## 深入了解

在C++中，日期通常以整数形式存储，表示从某个起始点（通常是某个特定的日期）到今天的天数。比如，20210516表示从公元1年1月1日开始到2021年5月16日的天数。我们可以使用不同的函数来获取日期，例如`time`和`localtime`。如果想要进行更复杂的日期比较，可以使用C++标准库中的`chrono`头文件来处理日期和时间计算。

## 参考资料

- [C++日期和时间处理教程](https://www.runoob.com/cplusplus/cpp-date-time.html)
- [C++标准库chrono头文件介绍](https://hoxis.github.io/cpp-intro/chrono.html)
- [C++标准库time头文件参考](https://www.cplusplus.com/reference/ctime/)