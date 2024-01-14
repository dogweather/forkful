---
title:    "Python: 将日期转换为字符串"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么
日期是我们日常生活中必不可少的一部分，我们经常需要将日期转换成字符串来满足不同的需求。例如，我们可能需要在程序中使用具有特定格式的日期字符串，或者将日期显示在用户界面上。无论是什么原因，Python中的日期转换到字符串功能可以帮助我们轻松地实现这些需求。

## 如何
``` python
# 导入datetime模块
import datetime

# 创建一个datetime对象
date = datetime.datetime(2021, 7, 1)

# 将日期转换成字符串
date_str = date.strftime("%Y年%m月%d日")

# 输出结果
print(date_str) # 2021年07月01日
```

在上面的代码示例中，我们首先导入了Python中用于处理日期和时间的datetime模块。接下来，我们创建了一个datetime对象并将它指定为2021年7月1日。然后，使用`strftime()`方法将日期转换成特定格式的字符串。注意，我们将日期格式作为参数传递给方法，这样我们就可以控制字符串的格式。最后，我们打印出日期字符串的结果。

除了上面使用的`%Y年%m月%d日`格式，datetime模块还有很多其他的格式选项，如`%B %d, %Y`表示月份的全称，日期的两位数表示，年份的四位数表示。具体的格式选项可以在Python官方文档中找到。

## 深入了解
在Python中，日期类型的对象有一个名为`__str__()`的内置方法，它可以将日期转换成字符串。`strftime()`方法其实就是调用了`__str__()`方法，只不过它提供了更多的格式选项。

另外，Python中的字符串也有一个叫做`strptime()`的方法，它可以将字符串转换成日期对象。这对于从用户输入的字符串中提取日期很有用。

## 参考链接
- [Python官方文档 - datetime模块](https://docs.python.org/3/library/datetime.html)
- [Python官方文档 - strftime及strptime格式](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)