---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u5982\u4F55\u505A\uFF1A Python \u4F7F\u5F97\u5C06\u65E5\u671F\u8F6C\
  \u6362\u4E3A\u5B57\u7B26\u4E32\u53D8\u5F97\u7B80\u5355\u3002\u4F7F\u7528[date](https://docs.python.org/3/library/datetime.html#date-\u2026"
lastmod: '2024-04-04T02:02:43.194394-06:00'
model: gpt-4-0125-preview
summary: "Python \u4F7F\u5F97\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\
  \u53D8\u5F97\u7B80\u5355\u3002\u4F7F\u7528[date](https://docs.python.org/3/library/datetime.html#date-objects)\u5BF9\
  \u8C61\u4E0A\u53EF\u7528\u7684[`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\u65B9\
  \u6CD5\u3002\u65B9\u6CD5\u5982\u4E0B\uFF1A."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何做：
Python 使得将日期转换为字符串变得简单。使用[date](https://docs.python.org/3/library/datetime.html#date-objects)对象上可用的[`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)方法。方法如下：

```Python
from datetime import datetime

# 获取当前的日期和时间
now = datetime.now()

# 将其转换为字符串格式：月 日, 年
date_string = now.strftime("%B %d, %Y")
print(date_string)  # 输出：March 29, 2023（或当前日期）

# 格式：YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # 输出：2023-03-29（或当前日期）
```


### 我是如何做的

这是我获取带有时区信息的[ISO 8601](https://www.w3.org/QA/Tips/iso-date)格式日期的方式：

```python
def datestamp() -> str:
    """ 
    带有时区的当前日期和时间，以ISO格式表示。
    """
    return datetime.now().astimezone().isoformat()
```

#### 示例输出：

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## 深入探讨
从历史上看，日期-字符串转换一直是编程中的一个基本需求，因为需要以人类可读的格式表示日期。

`strftime`的替代方法包括使用ISO 8601格式的`isoformat`方法，或使用如`arrow`和`dateutil`这样的第三方库，它们提供了更灵活的解析和格式化选项。

在实现上，`strftime`代表“字符串格式时间”，它源于C语言编程。Python的`strftime`解释了像`%Y`表示年份、`%m`表示月份等格式代码，允许几乎无限的自定义性。

## 另请参阅
要深入了解Python的日期和时间功能：
- Python官方的`datetime`文档：https://docs.python.org/3/library/datetime.html
- 对于那些对`strftime`指令感兴趣的完整列表：https://strftime.org/
- 探索第三方日期/时间库：
  - Arrow：https://arrow.readthedocs.io/en/latest/
  - python-dateutil：https://dateutil.readthedocs.io/en/stable/
