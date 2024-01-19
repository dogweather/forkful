---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
将日期转换为字符串是将日期对象转换为易于理解和读取的格式的操作。此操作在存储日期，将其作为参数传递，或将其显示给用户时极为重要。

## 如何：
Python 中有很多方法可以完成这项任务，但是今天我们将关注 `strftime` 方法。让我们看一些示例。

在 Python 中将 date object 转换为字符串：

```python
from datetime import date

d = date.today()
d_str = d.strftime("%Y-%m-%d")

print(d_str)
```

示例输出：

```python
'2022-03-20'
```

在这个例子中，`strftime` 方法返回一个表示日期的字符串。"%Y-%m-%d"是我们想要的日期格式，其中%Y，%m和%d分别代表年，月和日。

## 深度挖掘：
历史来看，`strftime` 方法的“strf”部分是“string format”的缩略语。这个方法最初是为了解决C语言中日期和时间格式化的问题而引入，Python也借用了这个方案。

另一个即使 `isoformat` 方法，它返回一个符合 ISO 8601 标准的日期字符串，大致相同，但结构不同。

```python
from datetime import date

d = date.today()
d_iso = d.isoformat()

print(d_iso)
```
示例输出：
```python
'2022-03-20'
```

这两种方法的实现都依赖于 C 库中的 `strftime` 函数，所以实际上，当你在 Python 中使用这些方法时，你在背后调用 C 代码。

## 另请参见：
1. Python strftime：https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior
2. ISO 8601 标准：https://www.iso.org/iso-8601-date-and-time-format.html
3. C 库中的 strftime：http://www.cplusplus.com/reference/ctime/strftime/