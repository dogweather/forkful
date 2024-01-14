---
title:                "Python: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Python 编程的**为什么**

计算未来或过去的日期是程序员们经常会遇到的任务之一。这可能是为了计划未来的活动，或者是处理历史数据。无论何种原因，掌握计算日期的能力都是非常有用的。

## 如何操作
为了计算未来或过去的日期，我们需要使用 Python 的 `datetime` 模块。首先，我们需要导入这个模块，如下所示：
```Python
import datetime
```
然后，我们可以使用 `today()` 方法来获取当前的日期，并将其存储在一个变量中：
```Python
current_date = datetime.date.today()
```
接下来，我们可以使用 `timedelta()` 方法来指定要添加或减去的时间间隔，比如下一个月或一个星期。然后，我们可以将其与当前日期相加或相减来获得未来或过去的日期。示例如下所示：
```Python
future_date = current_date + datetime.timedelta(days=30)
past_date = current_date - datetime.timedelta(days=14)
```
最后，我们可以使用 `strftime()` 方法将日期格式化为我们需要的形式。下面是一个完整的示例代码：
```Python
import datetime

current_date = datetime.date.today()
future_date = current_date + datetime.timedelta(days=30)
past_date = current_date - datetime.timedelta(days=14)

# 将日期格式化为“月-日-年”的形式
print("未来的日期：" + future_date.strftime("%m-%d-%Y"))
print("过去的日期：" + past_date.strftime("%m-%d-%Y"))
```
输出结果如下：
```
未来的日期：04-22-2021
过去的日期：03-09-2021
```

## 深入了解
除了添加和减去固定的时间间隔外，`timedelta()` 方法还可以指定其他参数，比如周数、小时数、分钟数等。此外，`strftime()` 方法也有很多不同的格式化选项可供选择。可以阅读官方文档来了解更多信息。

## 参考链接
- [Python 官方文档 - datetime 模块](https://docs.python.org/3/library/datetime.html)
- [Python 官方文档 - timedelta 对象](https://docs.python.org/3/library/datetime.html#timedelta-objects)
- [Python 官方文档 - strftime() 方法](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
 

## 参考

[Why People Calculate Dates in Python](https://realpython.com/python-datetime/)