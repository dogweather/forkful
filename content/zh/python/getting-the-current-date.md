---
title:                "获取当前日期"
html_title:           "Python: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么？
获取当前日期是指获取当前的日期，时间和时区。这对于程序员来说非常重要，因为他们需要跟踪和记录日期和时间以进行数据分析和错误调试。

## 如何：
使用Python编程语言，可以轻松地获取当前日期。以下是一个简单的代码示例来获取当前日期并将其打印输出：

```python
import datetime
today = datetime.date.today()
print(today)
```

输出将是类似于"2021-01-19"的当前日期格式。

## 深入探讨：
获取当前日期的方法已经存在了很长一段时间。在Python中，可以使用datetime模块来获取当前日期。如果你想要自定义日期格式，可以使用strptime()函数来解析日期字符串。此外，还有其他的Python库可以用来获取当前日期，例如calendar和arrow。

## 瞧一瞧：
想进一步了解如何获取当前日期和时间的更多方法，请参考以下链接：
- [Python datetime模块文档](https://docs.python.org/3/library/datetime.html)
- [Python calendar模块文档](https://docs.python.org/3/library/calendar.html)
- [Python arrow库](https://arrow.readthedocs.io/en/stable/)