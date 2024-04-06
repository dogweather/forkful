---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:25.524731-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A **\u4F7F\u7528\u6807\u51C6\u5E93`datetime`\uFF1A\
  ** Python\u6807\u51C6\u5E93\u4E2D\u7684`datetime`\u6A21\u5757\u63D0\u4F9B\u4E86\u7528\
  \u4E8E\u64CD\u4F5C\u65E5\u671F\u548C\u65F6\u95F4\u7684\u7C7B\u3002\u8981\u83B7\u53D6\
  \u5F53\u524D\u65E5\u671F\uFF0C\u53EF\u4EE5\u4F7F\u7528`date.today()`\u65B9\u6CD5\
  \u3002"
lastmod: '2024-04-05T22:38:46.446408-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A **\u4F7F\u7528\u6807\u51C6\u5E93`datetime`\uFF1A\
  ** Python\u6807\u51C6\u5E93\u4E2D\u7684`datetime`\u6A21\u5757\u63D0\u4F9B\u4E86\u7528\
  \u4E8E\u64CD\u4F5C\u65E5\u671F\u548C\u65F6\u95F4\u7684\u7C7B\u3002\u8981\u83B7\u53D6\
  \u5F53\u524D\u65E5\u671F\uFF0C\u53EF\u4EE5\u4F7F\u7528`date.today()`\u65B9\u6CD5\
  \u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
