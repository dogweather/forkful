---
title:                "获取当前日期"
date:                  2024-01-20T15:16:32.819526-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 为什么以及为什么？
获取当前日期就是让程序能知道今天是什么日子。程序员这么做是因为很多程序需要使用到实时日期数据，比如日志记录、数据分析或用户界面显示。

## How to: 怎么做
```Python
from datetime import date

today = date.today()
print("Today's date:", today)
```
输出样例：
```
Today's date: YYYY-MM-DD
```
换个方式，如果你想要详细的时间信息：
```Python
from datetime import datetime

now = datetime.now()
print("Current date and time:", now)
```
输出样例：
```
Current date and time: YYYY-MM-DD HH:MM:SS.microsecond
```

## Deep Dive 深入探讨
在Python的历史中，获取当前日期和时间的方法一直在进化。最早的`time`模块提供了基础功能，但是`datetime`模块的引入使得日期时间处理变得更强大、更直观。

`datetime`比`time`提供更丰富的对象和方法，如`date`, `time`, `datetime`, `timedelta`等。使用`datetime`模块，你可以轻松进行日期时间的计算、比较和格式化。

今天，`datetime.now()`和`date.today()`是两个常用的函数。`now()`返回当前的日期和时间，而`today()`只返回当前的日期。

除了`datetime`模块，第三方库如`arrow`, `dateutil`, `pendulum`也提供了更加高级和灵活的日期时间操作方法。

实现细节方面，Python内部是通过调用操作系统提供的日期时间函数来获取当前的日期时间的。这意味着，Python中的日期时间是和你电脑或服务器上的系统时间同步的。

## See Also 参考链接
- datetime模块官方文档: https://docs.python.org/3/library/datetime.html
- Python time模块官方文档: https://docs.python.org/3/library/time.html
- Arrow: https://arrow.readthedocs.io/en/latest/
- python-dateutil: https://dateutil.readthedocs.io/en/stable/
- Pendulum: https://pendulum.eustace.io/
