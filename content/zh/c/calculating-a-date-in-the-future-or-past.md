---
title:                "C: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要计算未来或过去的日期时，我们可以使用计算机语言来自动完成这一任务，使得这一过程更加高效和准确。

## 如何完成

在C编程中，我们可以使用函数`localtime()`来获取当前日期的结构化信息，并将其存储在一个`tm`类型的变量中。然后，我们可以使用该变量中的年、月、日等信息来计算未来或过去的日期。

例如，我们想要计算一年后的日期，我们可以添加365天到当前日期的年份，并确保月份和天数的正确性。下面是一个计算未来日期的简单示例：

```C
#include <stdio.h>
#include <time.h>

int main() {
  // 获取当前日期并存储在tm变量中
  time_t now = time(NULL);
  struct tm *current_date = localtime(&now);
  
  // 增加一年
  current_date->tm_year += 1;
  
  // 确保月份和天数的正确性
  mktime(current_date);
  
  // 打印计算出的日期
  printf("一年后的日期是：%d年 %d月 %d日\n", current_date->tm_year + 1900, current_date->tm_mon + 1, current_date->tm_mday);
  
  return 0;
}
```

输出：

```
一年后的日期是：2022年 4月 4日
```

## 深入探讨

在C编程中，我们也可以使用`strptime()`函数来将一个字符串转换为日期结构。这样，我们就可以使用用户输入的日期来计算未来或过去的日期。同时，我们也可以使用`strftime()`函数来将日期结构转换为指定格式的字符串。

但是，需要注意的是，在计算日期时，我们应该考虑闰年和月份的天数差异，以及日期的合法性。因此，我们需要仔细地阅读官方文档，以确保我们的代码能够正确地计算日期。

## 参考文献

- [C编程网页教程](https://www.runoob.com/cprogramming/c-tutorial.html)
- [C函数参考手册](https://www.cplusplus.com/reference/ctime/)
- [C语言编程实例](https://www.programiz.com/c-programming/examples/add-date)