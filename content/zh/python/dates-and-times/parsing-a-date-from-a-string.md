---
title:                "从字符串解析日期"
aliases: - /zh/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:56.823799-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
