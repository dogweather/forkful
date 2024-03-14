---
date: 2024-01-20 17:32:14.637923-07:00
description: "\u8BA1\u7B97\u5C06\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u6307\
  \u627E\u51FA\u5728\u7279\u5B9A\u65E5\u671F\u4E4B\u524D\u6216\u4E4B\u540E\u7684\u786E\
  \u5207\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\
  \u7406\u9884\u5B9A\u3001\u671F\u9650\u3001\u6216\u8005\u4EFB\u4F55\u8DDF\u65F6\u95F4\
  \u76F8\u5173\u7684\u529F\u80FD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.270730-06:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u5C06\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u6307\
  \u627E\u51FA\u5728\u7279\u5B9A\u65E5\u671F\u4E4B\u524D\u6216\u4E4B\u540E\u7684\u786E\
  \u5207\u65E5\u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\
  \u7406\u9884\u5B9A\u3001\u671F\u9650\u3001\u6216\u8005\u4EFB\u4F55\u8DDF\u65F6\u95F4\
  \u76F8\u5173\u7684\u529F\u80FD\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算将来或过去的日期是指找出在特定日期之前或之后的确切日期。程序员这么做是为了处理预定、期限、或者任何跟时间相关的功能。

## 如何做：
```Python
from datetime import datetime, timedelta

# 当前日期
now = datetime.now()
print("现在:", now.strftime("%Y-%m-%d"))

# 10天后的日期
future_date = now + timedelta(days=10)
print("未来10天:", future_date.strftime("%Y-%m-%d"))

# 10天前的日期
past_date = now - timedelta(days=10)
print("过去10天:", past_date.strftime("%Y-%m-%d"))
```
输出样例：
```
现在: 2023-03-25
未来10天: 2023-04-04
过去10天: 2023-03-15
```

## 深入探索
在Python早期版本中，处理日期通常更复杂，需要手动管理计算和转换。随着`datetime`和`timedelta`的引入，计算未来或过去的日期变得直接且容易。

除了`datetime`模块，还有其他库，如`dateutil`，可以处理复杂的日期问题，比如重复发生的事件和节假日。此外，时间处理还必须考虑时区和夏时制的影响。

在实际应用中，比如计算与当天的用户订阅过期剩余天数，或者计算产品送达的预期日期，均需要用到这些计算未来与过去日期的技能。

## 参考资料
- [Python `datetime` documentation](https://docs.python.org/3/library/datetime.html)
- [dateutil documentation](https://dateutil.readthedocs.io/en/stable/)
- [PyPI - Python Package Index](https://pypi.org/)  - 查找额外的日期时间相关库。
