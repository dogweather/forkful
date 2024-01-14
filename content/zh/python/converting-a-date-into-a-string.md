---
title:    "Python: 将日期转换为字符串"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 为什么
在编程中，有时需要将日期转换为字符串的格式。这在数据处理和可视化中特别有用，因为它使得日期更易于理解和操作。通过本文，您将学习如何利用Python将日期转换为字符串，并深入了解这个过程背后的原理。

## 如何操作
首先，我们需要导入Python自带的`datetime`库。然后，我们可以使用`strftime()`函数来将日期转换为字符串，其格式为`%Y-%m-%d %H:%M:%S`。让我们来看看下面的代码示例和输出结果：

```python
import datetime

current_date = datetime.datetime.now()
current_str = current_date.strftime("%Y-%m-%d %H:%M:%S")

print("当前日期：", current_date)
print("转换后的字符串：", current_str)
```

输出结果：
```
当前日期： 2021-08-09 16:28:42.154612
转换后的字符串： 2021-08-09 16:28:42
```

从上面的例子可以看出，`strftime`函数能够将日期转换为指定格式的字符串。您可以根据自己的需求调整日期的格式，例如`%d-%m-%Y`或`%H:%M`等等。

## 深入探讨
现在让我们来看看这个转换日期的过程背后的原理。在计算机中，日期和时间都是以数字的形式存储的，而人类更习惯于以可读的字符串来表示日期和时间。因此，通过`strftime`函数，我们可以根据指定的格式将数字转换为字符串。

此外，Python的`datetime`库中还提供了其他相关的函数，如`strptime`函数（字符串转换为日期）和`utctime`函数（将日期转换为UTC时间）。这些函数都可以帮助我们更轻松地处理日期和时间的转换。

## 参考资料
- [Python官方文档 - datetime模块](https://docs.python.org/3/library/datetime.html)
- [strftime方法文档](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Python3教程 - 日期和时间](https://www.runoob.com/python3/python3-date-time.html)

## 参考链接
- [Python - 将日期转换为字符串](https://www.programiz.com/python-programming/datetime/strftime)
- [Python日期转换方法总结](https://www.cnblogs.com/xiaoxiao_bai/p/13410726.html)
- [Python日期时间的转换与格式化](https://www.zhangshengrong.com/writing/python-date/)