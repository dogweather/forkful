---
title:                "Python: 获取当日日期"
simple_title:         "获取当日日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

获取当前日期对于编程来说是很重要的一件事情。无论是在编写日志文件、计算时间间隔还是进行数据分析，都需要准确地获取当前日期。

# 如何获取当前日期

在Python中，可以使用内置的datetime模块来获取当前日期。首先，需要导入datetime模块：

```python
import datetime
```

然后，使用`datetime.now()`方法即可获取当前日期时间，例如：

```python
current_date = datetime.now()
```

接下来，可以对获取到的日期进行格式化，例如将其转换为年月日的格式：

```python
formatted_date = current_date.strftime("%Y-%m-%d")
```

最后，我们可以将格式化后的日期打印出来，来验证是否得到了正确的输出：

```python
print(formatted_date)
```

输出结果将类似于`2021-01-01`，这就是我们准确地获取到了当前日期。

# 深入了解获取当前日期

除了直接使用`datetime.now()`方法来获取当前日期外，还可以使用其他方法来实现同样的功能。例如，可以使用`date.today()`方法来只获取日期，而不包含时间。

此外，还可以使用`calendar`模块来获取当前日期所在的星期几等信息。而如果要进行日期的加减运算，可以使用`timedelta`对象来实现，非常便捷。

# 参考资料

- [Python文档 - datetime模块](https://docs.python.org/3/library/datetime.html)
- [RealPython - Working with Dates and Times Using datetime](https://realpython.com/python-datetime/)
- [菜鸟教程 - Python日期和时间](https://www.runoob.com/python/python-datetime.html)

---

# 相关链接

- [Markdown基本语法](https://www.runoob.com/markdown/md-tutorial.html)
- [如何在VSCode中使用Markdown](https://code.visualstudio.com/docs/languages/markdown)
- [Github Markdown指南](https://guides.github.com/features/mastering-markdown/)