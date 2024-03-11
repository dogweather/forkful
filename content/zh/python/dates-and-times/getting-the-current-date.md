---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:25.524731-07:00
description: "\u5728Python\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u8BB8\u591A\
  \u5E94\u7528\u7A0B\u5E8F\u7684\u57FA\u672C\u64CD\u4F5C\uFF0C\u4F8B\u5982\u65E5\u5FD7\
  \u8BB0\u5F55\u3001\u6570\u636E\u5206\u6790\u548C\u57FA\u4E8E\u65F6\u95F4\u7684\u51B3\
  \u7B56\u5236\u5B9A\u3002\u5B83\u662F\u5173\u4E8E\u68C0\u7D22\u7CFB\u7EDF\u7684\u5F53\
  \u524D\u65E5\u671F\uFF0C\u8FD9\u5BF9\u4E8E\u4F9D\u8D56\u65F6\u95F4\u80CC\u666F\u7684\
  \u4EFB\u52A1\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-11T00:14:21.034796-06:00'
model: gpt-4-0125-preview
summary: "\u5728Python\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u8BB8\u591A\
  \u5E94\u7528\u7A0B\u5E8F\u7684\u57FA\u672C\u64CD\u4F5C\uFF0C\u4F8B\u5982\u65E5\u5FD7\
  \u8BB0\u5F55\u3001\u6570\u636E\u5206\u6790\u548C\u57FA\u4E8E\u65F6\u95F4\u7684\u51B3\
  \u7B56\u5236\u5B9A\u3002\u5B83\u662F\u5173\u4E8E\u68C0\u7D22\u7CFB\u7EDF\u7684\u5F53\
  \u524D\u65E5\u671F\uFF0C\u8FD9\u5BF9\u4E8E\u4F9D\u8D56\u65F6\u95F4\u80CC\u666F\u7684\
  \u4EFB\u52A1\u81F3\u5173\u91CD\u8981\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
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
