---
title:                "从字符串解析日期"
date:                  2024-01-20T15:37:58.173223-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"

category:             "Python"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么?
解析日期就是将字符串转换成日期对象。程序员这么做是为了方便处理和比较日期数据。

## How to 如何操作
使用 Python 中的 `datetime` 模块，你可以轻松将字符串解析为日期。

```Python
from datetime import datetime

# 标准格式 YYYY-MM-DD
date_string = "2023-04-03"
parsed_date = datetime.strptime(date_string, "%Y-%m-%d")

print(parsed_date)  # 输出: 2023-04-03 00:00:00
```

## Deep Dive 深入了解
日期解析在编程中很常见。在 Python 中，常用 `datetime.strptime()` 函数解析日期。`strptime()` 是 "string parse time" 的缩写。`datetime` 是 Python 标准库中的一个模块，自 Python 2.3 起就存在了。

解析日期的替代方法包括使用第三方库，如 `dateutil`，它能处理更多不标准的日期格式。

```Python
from dateutil import parser

date_string = "April 3, 2023 14:00"
parsed_date = parser.parse(date_string)

print(parsed_date)  # 输出: 2023-04-03 14:00:00
```

了解不同的格式化代码（如 `%Y`, `%m`, `%d` 等）也很重要，这些都是指定日期组件的方式。

## See Also 另见
- Python 官方文档关于 `datetime` 模块: https://docs.python.org/3/library/datetime.html
- `dateutil` 库官方文档: https://dateutil.readthedocs.io/en/stable/
- Python 格式化时间字符串的指南: https://strftime.org/
