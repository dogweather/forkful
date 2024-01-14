---
title:                "Python: 获取当前日期"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：获取当前日期的原因
获取当前日期是一个非常有用的技巧。在Python编程中，我们经常需要获取今天的日期，例如记录数据或者生成文件名。通过学习如何获取当前日期，你可以轻松地应用到自己的项目中。

怎么做：获取当前日期的方法
```Python
# 导入datetime模块
import datetime

# 使用datetime模块中的date方法获取当前日期
today = datetime.date.today()

# 打印输出今天的日期
print("今天的日期是：", today)
```

输出：
```
今天的日期是：2021-09-26
```

深入了解：获取当前日期的更多信息
Python中的datetime模块提供了许多有用的方法来处理日期和时间。对于获取当前日期，我们可以使用`date.today()`方法来获取一个对应当天日期的`date`对象。此外，我们也可以通过`now()`方法来获取当前的日期和时间，或者使用`strftime()`方法来自定义日期的格式。

另外，Python中还有一个`calendar`模块，它可以帮助我们处理更复杂的日期操作，如获取一个月的日历或者判断某一年是否是闰年。

总之，掌握如何获取当前日期是十分重要的，它可以让你的日常编程更加高效和便捷。

参考链接：
- [Python datetime模块文档](https://docs.python.org/3/library/datetime.html)
- [Python calendar模块文档](https://docs.python.org/3/library/calendar.html)

另请参阅：
个人博客、Python官方文档、Stack Overflow等相关资源。