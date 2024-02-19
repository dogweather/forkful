---
aliases:
- /zh/python/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:56.823799-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\
  \u6587\u672C\u4E2D\u7684\u65E5\u671F\u548C\u65F6\u95F4\u4FE1\u606F\u8F6C\u6362\u4E3A\
  \ datetime \u5BF9\u8C61\u6216\u7B49\u6548\u7684\u7ED3\u6784\u5316\u683C\u5F0F\u3002\
  \u8FD9\u901A\u5E38\u662F\u4E3A\u4E86\u4F7F\u65E5\u671F\u8FD0\u7B97\u3001\u6BD4\u8F83\
  \u548C\u683C\u5F0F\u5316\u64CD\u4F5C\u4EE5\u4E00\u79CD\u8BED\u8A00\u548C\u533A\u57DF\
  \u65E0\u5173\u7684\u65B9\u5F0F\u8FDB\u884C\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u6709\u6548\u5730\u5904\u7406\u548C\u64CD\u4F5C\u4ECE\
  \u65E5\u5FD7\u3001\u7528\u6237\u8F93\u5165\u6216\u5916\u90E8\u6E90\u4E2D\u63D0\u53D6\
  \u7684\u65F6\u95F4\u6570\u636E\u3002"
lastmod: 2024-02-18 23:08:58.799006
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u5C06\
  \u6587\u672C\u4E2D\u7684\u65E5\u671F\u548C\u65F6\u95F4\u4FE1\u606F\u8F6C\u6362\u4E3A\
  \ datetime \u5BF9\u8C61\u6216\u7B49\u6548\u7684\u7ED3\u6784\u5316\u683C\u5F0F\u3002\
  \u8FD9\u901A\u5E38\u662F\u4E3A\u4E86\u4F7F\u65E5\u671F\u8FD0\u7B97\u3001\u6BD4\u8F83\
  \u548C\u683C\u5F0F\u5316\u64CD\u4F5C\u4EE5\u4E00\u79CD\u8BED\u8A00\u548C\u533A\u57DF\
  \u65E0\u5173\u7684\u65B9\u5F0F\u8FDB\u884C\u3002\u7A0B\u5E8F\u5458\u6267\u884C\u6B64\
  \u64CD\u4F5C\u662F\u4E3A\u4E86\u6709\u6548\u5730\u5904\u7406\u548C\u64CD\u4F5C\u4ECE\
  \u65E5\u5FD7\u3001\u7528\u6237\u8F93\u5165\u6216\u5916\u90E8\u6E90\u4E2D\u63D0\u53D6\
  \u7684\u65F6\u95F4\u6570\u636E\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么与为什么？
从字符串中解析日期涉及将文本中的日期和时间信息转换为 datetime 对象或等效的结构化格式。这通常是为了使日期运算、比较和格式化操作以一种语言和区域无关的方式进行。程序员执行此操作是为了有效地处理和操作从日志、用户输入或外部源中提取的时间数据。

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
