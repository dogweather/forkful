---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:56.823799-07:00
description: "\u5982\u4F55\u505A\uFF1A Python \u7684\u6807\u51C6\u5E93\u63D0\u4F9B\
  \u4E86 `datetime` \u6A21\u5757\uFF0C\u5176\u4E2D\u5305\u62EC\u4E86\u7528\u4E8E\u6B64\
  \u76EE\u7684\u7684 `strptime` \u65B9\u6CD5\u3002\u8BE5\u65B9\u6CD5\u9700\u8981\u4E24\
  \u4E2A\u53C2\u6570\uFF1A\u65E5\u671F\u5B57\u7B26\u4E32\u548C\u4E00\u4E2A\u683C\u5F0F\
  \u6307\u4EE4\uFF0C\u7528\u4E8E\u6307\u5B9A\u8F93\u5165\u5B57\u7B26\u4E32\u7684\u6A21\
  \u5F0F\u3002"
lastmod: '2024-03-13T22:44:47.266570-06:00'
model: gpt-4-0125-preview
summary: "Python \u7684\u6807\u51C6\u5E93\u63D0\u4F9B\u4E86 `datetime` \u6A21\u5757\
  \uFF0C\u5176\u4E2D\u5305\u62EC\u4E86\u7528\u4E8E\u6B64\u76EE\u7684\u7684 `strptime`\
  \ \u65B9\u6CD5\u3002\u8BE5\u65B9\u6CD5\u9700\u8981\u4E24\u4E2A\u53C2\u6570\uFF1A\
  \u65E5\u671F\u5B57\u7B26\u4E32\u548C\u4E00\u4E2A\u683C\u5F0F\u6307\u4EE4\uFF0C\u7528\
  \u4E8E\u6307\u5B9A\u8F93\u5165\u5B57\u7B26\u4E32\u7684\u6A21\u5F0F."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何做：
Python 的标准库提供了 `datetime` 模块，其中包括了用于此目的的 `strptime` 方法。该方法需要两个参数：日期字符串和一个格式指令，用于指定输入字符串的模式。

```python
from datetime import datetime

# 示例字符串
date_string = "2023-04-01 14:30:00"
# 将字符串解析为 datetime 对象
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# 输出：2023-04-01 14:30:00
```

对于更细致的日期解析，尤其是在处理多种格式或地区设置时，第三方库 `dateutil` 可以大有帮助。它提供了一个解析器模块，可以解析几乎任何字符串格式的日期。

```python
from dateutil import parser

# 示例字符串
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# 使用 dateutil 的解析器
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# 输出：2023-04-01 14:30:00
print(parsed_date2)
# 输出：2023-04-01 14:30:00
```

`dateutil` 在没有显式格式字符串的情况下擅长处理大多数日期格式，使其成为处理多样化日期表示的应用程序的多功能选择。
