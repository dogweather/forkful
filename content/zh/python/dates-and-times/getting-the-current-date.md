---
title:                "获取当前日期"
aliases:
- /zh/python/getting-the-current-date.md
date:                  2024-02-03T19:10:25.524731-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

在Python中获取当前日期是许多应用程序的基本操作，例如日志记录、数据分析和基于时间的决策制定。它是关于检索系统的当前日期，这对于依赖时间背景的任务至关重要。

## 如何操作：

**使用标准库`datetime`：**

Python标准库中的`datetime`模块提供了用于操作日期和时间的类。要获取当前日期，可以使用`date.today()`方法。

```python
from datetime import date

today = date.today()
print(today)  # 输出：YYYY-MM-DD（例如，2023-04-05）
```

**时间格式化：**

如果您需要不同格式的当前日期，`strftime`方法允许您指定自定义日期格式：

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # 示例格式："April 05, 2023"
print(formatted_date)
```

**使用`pendulum`获得更多灵活性（一个流行的第三方库）：**

`Pendulum`是一个第三方库，它提供了一种更直观的方法来处理Python中的日期和时间。它扩展了标准datetime功能，并简化了时区管理等功能。

首先，确保您通过pip安装了`pendulum`：

```shell
pip install pendulum
```

然后，获取当前日期：

```python
import pendulum

today = pendulum.now().date()
print(today)  # 输出：YYYY-MM-DD（例如，2023-04-05）
```

使用`pendulum`，格式化也很简单，类似于`strftime`方法：

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # 默认格式："Apr 5, 2023"
print(formatted_date)
```
