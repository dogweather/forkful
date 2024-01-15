---
title:                "比较两个日期"
html_title:           "Python: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

比较两个日期在编程中非常常见，因为我们经常需要根据日期来做出决策或者对数据进行筛选和分析。比较两个日期可以帮助我们更容易地处理日期相关的任务，提高我们的编程效率。

## 怎么做

```Python
# 导入datetime库
import datetime

# 定义两个日期变量
date1 = datetime.date(2021, 1, 1)
date2 = datetime.date(2021, 5, 1)

# 比较两个日期是否相等
if date1 == date2:
    print("日期相等")
else:
    print("日期不相等")

# 比较两个日期的大小
if date1 < date2:
    print("date1 在 date2 之前")
elif date1 > date2:
    print("date1 在 date2 之后")
else:
    print("日期相等")
```

输出：
```
日期不相等
date1 在 date2 之前
```

## 深入了解

Python中有两个内置的日期数据类型，一个是`date`类型，表示日期，一个是`datetime`类型，表示日期和时间。当我们比较两个日期时，通常使用的是`date`类型。

比较日期的本质是比较日期的大小，而日期的大小是根据日期所在的位置来决定的。比如，2021年1月1日比2021年5月1日要早，所以前者小于后者。

除了直接比较日期的大小，我们还可以使用`timedelta`来计算两个日期之间的时间差。

参考文档：
- [Python官方文档 - Datetime](https://docs.python.org/3/library/datetime.html)
- [菜鸟教程 - Python日期和时间](https://www.runoob.com/python/python-date-time.html)

## 参考资料

[为什么你应该了解并且掌握Python中的日期比较](https://blog.csdn.net/mingzznet/article/details/90808781)

## 参见

[Python中日期的格式化输出](https://github.com/HelenAndJunior/python-examples/blob/master/dates/formatting_dates.md)

[如何在Python中获取当前日期和时间](https://github.com/HelenAndJunior/python-examples/blob/master/dates/current_datetime.md)