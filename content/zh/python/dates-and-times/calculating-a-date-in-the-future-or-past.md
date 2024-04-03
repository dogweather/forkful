---
date: 2024-01-20 17:32:14.637923-07:00
description: "\u5982\u4F55\u505A\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.270730-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
weight: 26
---

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
