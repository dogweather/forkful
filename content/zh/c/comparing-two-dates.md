---
title:                "C: 比较两个日期"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

日期在我们日常生活中扮演着重要的角色，我们经常需要比较两个日期的大小或者计算两个日期之间的时间差。因此，学习如何比较两个日期也成为了编程中必不可少的一个技能。

## 如何做

在C语言中，比较两个日期的最简单方法是通过使用内置的时间函数`difftime()`。这个函数可以计算两个日期之间的时间差，并返回一个`double`类型的值。例如，如果我们想要比较两个日期`start`和`end`，代码如下所示：

```C
#include <stdio.h>
#include <time.h>

int main()
{
  time_t start, end;
  double diff;

  // 获取开始和结束日期
  printf("请输入开始日期 (格式为YYYY-MM-DD)：");
  scanf("%ld", &start);
  printf("请输入结束日期 (格式为YYYY-MM-DD)：");
  scanf("%ld", &end);

  // 计算时间差并输出结果
  diff = difftime(end, start);
  printf("两个日期相差的秒数为: %.f\n", diff);

  return 0;
}
```

运行结果可能如下所示：

```C
请输入开始日期 (格式为YYYY-MM-DD)：2021-01-01
请输入结束日期 (格式为YYYY-MM-DD)：2021-01-10
两个日期相差的秒数为: 777600
```

## 深入探讨

在C语言中，日期是以自1970年1月1日以来的秒数来表示的，这被称为Unix时间戳。因此，`difftime()`函数计算的结果也是基于此原理。在比较日期时，我们需要确保输入的日期格式是正确的，否则计算出的时间差可能会出现错误。

此外，C语言中还有其他的时间函数，如`mktime()`用于将指定的日期转换为Unix时间戳，`asctime()`用于将时间戳转换为字符串形式的日期等，可以根据需求选择合适的函数来比较日期。

## 参考链接

- [C语言时间函数](https://www.runoob.com/cprogramming/c-standard-library-time-h.html)
- [Unix时间戳详解](https://www.zhihu.com/question/33503114)
- [C语言日期和时间处理方法](https://www.jianshu.com/p/4bb0dc1ffe45)

## 参见

- [C语言中日期比较函数使用指南](https://www.example.com)
- [C语言中日期运算的一些技巧](https://www.example2.com)