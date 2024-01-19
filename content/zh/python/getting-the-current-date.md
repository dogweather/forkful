---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么与为什么？
获取当前日期为取得当前时刻的日期信息。程序员之所以要获取当前日期，主要是为了在数据分析、时间记录和定时任务等场景中使用。

## 如何操作：
Python提供了日期和时间操作的内置库，名为`datetime`。以下示例演示了如何使用`datetime`库获取当前日期。

```Python
from datetime import date

# 获取今天的日期
today = date.today()
print("今天的日期是:", today)
```

运行上述代码，输出类似这样的结果：
```Python
今天的日期是: 2022-06-06
```

## 深度探索：
### 历史背景
Python从一开始就引入了`datetime`模块，以便开发者处理日期和时间。`date.today()`函数用于获取当前的日期。

### 可选方法
除了使用Python内置的`datetime`模块，你还可以使用像是pandas这样的第三方库来获取当前日期，尤其在进行数据分析的时候这会特别有用。

```Python
import pandas as pd

# 获取今天的日期
today = pd.to_datetime("today").date()
print("今天的日期是:", today)
```

### 执行细节
在Python中获取当前日期的标准方法主要涉及`datetime`模块中的`date`类和其`today()`方法。此方法将返回一个`date`对象，包含了当前日期的年份、月份和日期。

## 另请参阅：
使用Python处理日期和时间的相关资源：
1. Python官方文档 - datetime模块：https://docs.python.org/3/library/datetime.html
2. Python日期和时间：https://www.w3schools.com/python/python_datetime.asp
3. Pandas to_datetime函数文档：https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html