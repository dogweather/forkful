---
title:                "计算未来或过去的日期"
html_title:           "Python: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算未来或过去的日期可能是帮助人们规划日程表或安排活动的一种方式。Python提供了方便的功能来执行此任务，让我们来深入了解一下吧！

## 如何操作

### 1. 导入datetime模块

首先，我们需要导入Python的datetime模块，它包含了许多有用的函数来处理日期和时间。

```Python
import datetime
```

### 2. 获取当前日期

要获取当前日期，我们可以使用datetime模块中的datetime类的today()函数。

```Python
today = datetime.today()
print(today)
```

输出：

```
2021-08-05 21:43:08.638832
```

### 3. 计算未来或过去的日期

要计算未来的日期，我们可以使用datetime模块中的timedelta类的函数。 timdelta类可以用来表示日期之间的差距。

下面的示例中，我们使用timedelta来计算当前日期后的3天的日期。

```Python
three_days_later = today + datetime.timedelta(days=3)
print(three_days_later)
```

输出：

```
2021-08-08 21:43:08.638832
```

同样的，我们也可以计算过去的日期，只需将timedelta的参数改为负数。

```Python
three_days_ago = today - datetime.timedelta(days=3)
print(three_days_ago)
```

输出：

```
2021-08-02 21:43:08.638832
```

## 深入了解

datetime模块中还有许多其他有用的函数，例如strftime()函数可以将日期转换为不同的格式，strptime()函数可以将字符串转换为datetime对象。对于需要处理日期和时间的任务，这些函数都是非常实用的。

此外，Python还有第三方模块dateutil，它提供了更多更灵活的日期和时间操作功能，具体可以查看它的官方文档。

## 参考链接

- [Python datetime模块文档](https://docs.python.org/3/library/datetime.html)
- [dateutil官方文档](https://dateutil.readthedocs.io/en/stable/index.html)

## 参考

- [Python 100天 - 第26天：datetime模块](https://www.bilibili.com/video/BV1m7411J7Es?p=26)