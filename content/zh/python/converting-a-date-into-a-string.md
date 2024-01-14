---
title:                "Python: 将日期转换成字符串"
simple_title:         "将日期转换成字符串"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编程的世界里，我们常常需要将日期转换成字符串。这样做的好处是，我们可以更方便地处理日期数据，并在输出时满足程序的要求。在这篇博文中，我将向大家介绍如何使用Python进行日期转换，并深入探讨其中的细节。

## 如何操作

要将日期转换成字符串，我们需要用到Python的日期和时间模块——datetime。下面是一个简单的示例代码：

```Python
import datetime

# 创建一个日期对象
date = datetime.datetime(2021, 8, 17)

# 将日期转换成指定格式的字符串
string_date = date.strftime("%Y-%m-%d") 
# 输出结果为： "2021-08-17"
```

在这个示例中，我们首先导入了datetime模块，然后使用datetime.datetime()来创建一个日期对象。接着，我们使用strftime()方法，指定了想要的日期格式，并将日期转换成了字符串。

除了指定日期格式外，还可以根据需求添加其他的输出信息，比如星期几、上午/下午等。下面是一个带有更多信息的例子：

```Python
import datetime

date = datetime.datetime(2021, 8, 17)

# 将日期转换成字符串并添加星期几和上午/下午信息
string_date = date.strftime("%Y年%m月%d日 %A %p")
# 输出结果为： "2021年08月17日 星期二 下午"
```

另外，如果想要将现在的日期转换成字符串，可以使用datetime模块中的now()方法，如下所示：

```Python
import datetime

# 获取当前日期
current_date = datetime.datetime.now()

# 将日期转换成字符串
string_date = current_date.strftime("%Y-%m-%d")
# 输出结果为：当前日期的字符串形式，例如："2021-08-17"
```

## 深入探讨

在Python中，日期和时间被表示为一个datetime对象，这个对象包含年、月、日、时、分、秒等信息。要将这些信息转换成字符串，就需要使用strftime()方法来指定输出格式。

strftime()方法中的格式字符串是一个定义日期和时间的标准字符串。其中，%Y代表四位数的年份，%m代表两位数的月份，%d代表两位数的日期，%A代表星期几，%p代表上午/下午等。对于更多的格式信息，可以参考Python官方文档。

除了strftime()方法，Python还提供了其他方法来转换日期，比如使用str()函数、使用format()方法等。

## 参考链接

- [Python官方文档（datetime模块）](https://docs.python.org/3/library/datetime.html)
- [Python strftime()方法格式指南](https://strftime.org/)