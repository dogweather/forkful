---
title:                "获取当前日期"
html_title:           "C: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

当我们写程序时，经常需要使用当前的日期作为标记，记录数据或作为条件来执行特定的代码。因此，了解如何获取当前日期在编程中是非常重要的。

## 如何去做

要获取当前日期，我们可以使用C语言的`time.h`头文件中的`time()`函数。让我们来看一个简单的例子：

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL);
  printf("当前日期为：%s\n", ctime(&now));
  return 0;
}
```

输出：

```
当前日期为：Fri Sep 24 20:28:36 2021
```

在这个例子中，我们首先引入`stdio.h`和`time.h`头文件，然后使用`time()`函数获取当前日期并存储在`now`变量中。接着，我们使用`ctime()`函数将`now`变量转换为字符串格式并打印出来。就是这么简单！

除了`ctime()`函数外，我们还可以使用`localtime()`和`strftime()`函数来获取和格式化当前日期。让我们看看下面这个例子：

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t now = time(NULL);
  struct tm *time_info = localtime(&now);
  char time_string[50];
  strftime(time_string, sizeof(time_string), "%Y-%m-%d %H:%M:%S", time_info);
  printf("当前日期为：%s\n", time_string);
  return 0;
}
```

输出：

```
当前日期为：2021-09-24 20:42:55
```

在这个例子中，我们通过`localtime()`函数将当前日期存储在`time_info`结构体中，然后使用`strftime()`函数按照指定的格式，将日期转换为字符串并存储在`time_string`变量中，最后打印出来。

## 深入了解

在C语言中，当前日期是通过从1970年1月1日起经过的秒数来表示的，这个日期称为“Unix时间戳”。因此，当我们使用`time()`函数获取当前日期时，实际上是获取的当前时刻与1970年1月1日0时0分0秒的时间差。

另外，需要注意的是，`ctime()`、`localtime()`和`gmtime()`等函数都有一个局限性，它们在处理日期时只能精确到秒，无法获取更细粒度的时间信息。如果我们需要毫秒级别的时间戳，可以使用`gettimeofday()`函数来替代。

## 参考资料

- [《C语言程序设计》（第4版）](https://book.douban.com/subject/1244636/)
- [《C语言程序设计专题讲解》](https://book.douban.com/subject/3808197/)
- [C语言教程 - 廖雪峰的官方网站](https://www.liaoxuefeng.com/wiki/1252599548343744/1255882234450080)