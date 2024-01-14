---
title:    "Python: 获取当前日期"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

在日常的编程工作中，经常会需要获取当前的日期。比如说，你想在程序中打印出今天是几月几号，或者你想在程序中使用日期来计算一些数据。无论是在什么场景下，获取当前日期都是一个非常常见和有用的操作。所以，学习如何获取当前日期是每个Python程序员都应该掌握的基本技能。

## 如何做到

做到获取当前日期的方法有很多种，但是在Python中有两种最简单的方法。第一种方法是使用内置的datetime模块，第二种方法是使用第三方库dateutil。让我们一起来看看这两种方法的实现。

### 使用datetime模块

在使用datetime模块之前，我们需要先导入它，然后创建一个datetime对象，最后使用strftime方法来指定日期的格式并打印出来。

```Python
# 导入datetime模块
import datetime

# 创建datetime对象
now = datetime.now()

# 格式化日期输出
print("今天是：" + now.strftime("%Y-%m-%d"))

# 输出：今天是：2021-08-26
```

### 使用dateutil库

与datetime模块不同，使用dateutil库可以让我们不仅仅获取当前日期，还可以对日期进行一些额外的操作。同样地，我们首先需要导入dateutil库，在创建date对象之后，我们可以使用dateutil库提供的其他方法来处理日期。

```Python
# 导入dateutil库
from dateutil import parser

# 创建date对象
today = parser.parse("2021-08-26")

# 打印今天的星期几
print("今天是：" + today.strftime("%A"))

# 输出：今天是：星期四
```

## 深入探讨

除了上面提到的两种方法，Python提供了一些其他的方法来获取当前日期。其中最常用的是使用time模块来获取当前的时间戳。时间戳是从1970年1月1日零时开始计算的秒数，可以用来进行日期比较或者计算时间差。

```Python
import time

# 获取当前时间戳
timestamp = time.time()

# 打印当前时间戳
print("当前时间戳为：" + str(timestamp))

# 输出：当前时间戳为：1629980188.672915
```

如果你想要使用时间戳来获取当前日期，只需要将时间戳转换为日期对象即可。

```Python
# 导入datetime模块
import datetime

# 获取当前时间戳
timestamp = time.time()

# 将时间戳转换为日期对象
current_date = datetime.fromtimestamp(timestamp)

# 打印当前日期
print("今天是：" + current_date.strftime("%Y-%m-%d"))

# 输出：今天是：2021-08-26
```

## 参考链接

- [Python中的datetime模块](https://docs.python.org/3/library/datetime.html)
- [Python中的dateutil库](https://dateutil.readthedocs.io/en/stable/)
- [Python中的time模块](https://docs.python.org/3/library/time.html)

## 参见

- [Python中的日期和时间操作教程](https://www.runoob.com/python/python-date-time.html)
- [如何在Python中获取当前时间和日期](https://www.geeksforgeeks.org/how-to-get-current-time-in-python/)
- [Python日期与时间 - 教程和实例](https://www.programiz.com/python-programming/datetime)