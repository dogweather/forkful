---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，比较两个日期的含义是根据年、月、日的值来决定这两个日期的顺序。这在开发事件调度，日历，待办事项列表，时间间隔计算等程序中非常常见。

## 如何操作：

Here's a quick way to compare two dates in Python.

```Python
from datetime import datetime

# Define two dates
date1 = datetime(2021, 2, 3)
date2 = datetime(2022, 2, 3)

# Compare dates
if date1 < date2:
    print("date1 is earlier")
else:
    print("date2 is earlier or dates are same")
```

When you run this code, you get:

```Python
date1 is earlier
```

## 深入探讨

Python的 `datetime` 库起源于 Effbot.org 在1999年的资助下进行的一个项目，它增加了时间和日期处理的效率。其它库如 `dateutil` 可以提供更多的功能，如时区处理，但对日期之间的简单比较来说可能过于复杂。虽然你可以手动比较每一个部分（年、月、日），但使用 `datetime` 是最简洁，最直观的方法。

## 参考资料

另请参阅 [Python `datetime` documentation](https://docs.python.org/3/library/datetime.html) 获取更多深入的信息和例子，参阅 [Python `dateutil` documentation](https://dateutil.readthedocs.io/en/stable/) 了解一个更复杂、更强大的日期与时间处理库。