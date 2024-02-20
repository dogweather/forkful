---
date: 2024-01-20 17:37:19.971492-07:00
description: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\u610F\u5473\u7740\
  \u628A\u65E5\u671F\u6570\u636E\u683C\u5F0F\u6539\u6210\u6587\u672C\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3A\u4E86\u663E\u793A\u6570\u636E\u3001\u5B58\
  \u50A8\u6216\u65E5\u5FD7\u8BB0\u5F55\u7B49\u65B9\u4FBF\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.348608
model: gpt-4-1106-preview
summary: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32\u610F\u5473\u7740\
  \u628A\u65E5\u671F\u6570\u636E\u683C\u5F0F\u6539\u6210\u6587\u672C\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u4E3A\u4E86\u663E\u793A\u6570\u636E\u3001\u5B58\
  \u50A8\u6216\u65E5\u5FD7\u8BB0\u5F55\u7B49\u65B9\u4FBF\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
将日期转换为字符串意味着把日期数据格式改成文本形式。程序员这样做为了显示数据、存储或日志记录等方便。

## How to: 怎么做？
```Python
from datetime import datetime

# 当前日期和时间
now = datetime.now()

# 转换为字符串
date_string = now.strftime("%Y-%m-%d %H:%M:%S")

print(date_string)  # 示例输出：2023-03-15 14:45:30
```

## Deep Dive 深入探讨
在Python中，日期和时间是由`datetime`模块处理的。`strftime`方法可以指定日期和时间的格式。它起源于C语言的标准库函数`strftime()`，现已成为多数编程语言的标准部分。

除了`strftime`，还有多种方式可以转换日期为字符串。例如，使用`isoformat()`方法可以生成一个ISO 8601格式的日期字符串：

```Python
iso_date_string = now.isoformat()
print(iso_date_string)  # 示例输出：2023-03-15T14:45:30.000001
```

在实际应用中，选择哪种转换方法取决于你的需要。是否需要与其他系统集成、数据存储的格式要求，或者个人偏好都可能影响决策。

## See Also 相关链接
- Python官方文档中的`datetime`模块：https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior
- strftime()和strptime()行为：https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
- ISO 8601日期和时间格式指南：https://en.wikipedia.org/wiki/ISO_8601
