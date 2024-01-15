---
title:                "将日期转换为字符串。"
html_title:           "Python: 将日期转换为字符串。"
simple_title:         "将日期转换为字符串。"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 为什么

有时候在编程中，我们需要将日期转换为字符串的形式。这可以用于将日期显示给用户，或者用于数据存储和处理。

# 如何

```Python
# 导入datetime模块
import datetime

# 创建一个date对象，包含年、月、日
date = datetime.date(2021, 10, 20)

# 使用strftime()函数将日期转换为字符串，指定格式为月/日/年
date_str = date.strftime("%m/%d/%Y")

# 打印结果
print(date_str)
```

输出结果为 `10/20/2021`。

# 深入了解

日期对象在Python中可以使用 `datetime` 模块创建。`strftime()` 函数可以将日期对象转换为指定格式的字符串。常用的格式包括 `%Y` （4位数年份），`%m` （2位数月份），`%d` （2位数日期）等。

日期对象也可以使用 `str()` 函数转换为字符串，但是它默认输出的格式并不是我们通常想要的，因此最好还是使用 `strftime()` 函数指定格式。

# 参考

- [Python官方文档 - datetime模块](https://docs.python.org/3/library/datetime.html)
- [Python官方文档 - strftime()函数](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)