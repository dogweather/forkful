---
date: 2024-01-20 17:32:14.637923-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728Python\u65E9\u671F\u7248\u672C\u4E2D\uFF0C\
  \u5904\u7406\u65E5\u671F\u901A\u5E38\u66F4\u590D\u6742\uFF0C\u9700\u8981\u624B\u52A8\
  \u7BA1\u7406\u8BA1\u7B97\u548C\u8F6C\u6362\u3002\u968F\u7740`datetime`\u548C`timedelta`\u7684\
  \u5F15\u5165\uFF0C\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u53D8\
  \u5F97\u76F4\u63A5\u4E14\u5BB9\u6613\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.623314-06:00'
model: gpt-4-1106-preview
summary: "\u9664\u4E86`datetime`\u6A21\u5757\uFF0C\u8FD8\u6709\u5176\u4ED6\u5E93\uFF0C\
  \u5982`dateutil`\uFF0C\u53EF\u4EE5\u5904\u7406\u590D\u6742\u7684\u65E5\u671F\u95EE\
  \u9898\uFF0C\u6BD4\u5982\u91CD\u590D\u53D1\u751F\u7684\u4E8B\u4EF6\u548C\u8282\u5047\
  \u65E5\u3002\u6B64\u5916\uFF0C\u65F6\u95F4\u5904\u7406\u8FD8\u5FC5\u987B\u8003\u8651\
  \u65F6\u533A\u548C\u590F\u65F6\u5236\u7684\u5F71\u54CD."
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
