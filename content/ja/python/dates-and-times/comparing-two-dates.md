---
date: 2024-01-20 17:33:33.436783-07:00
description: "How to: (\u65B9\u6CD5) Python's built-in `datetime` module has been\
  \ around since version 2.3. It provides objects for date and time handling. Before\
  \ `datetime`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.518948-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Python's built-in `datetime` module has been around since\
  \ version 2.3."
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (方法)
```Python
from datetime import datetime

# Create two date objects
date1 = datetime(2023, 3, 14)
date2 = datetime(2023, 4, 18)

# Compare the dates
print("Is date1 before date2?", date1 < date2)  # True
print("Is date1 after date2?", date1 > date2)   # False
print("Are both dates the same?", date1 == date2)  # False
```

Sample output:

```
Is date1 before date2? True
Is date1 after date2? False
Are both dates the same? False
```

## Deep Dive (深掘り)
Python's built-in `datetime` module has been around since version 2.3. It provides objects for date and time handling. Before `datetime`, programmers used time tuples or third-party libraries. As alternatives, the `dateutil` library offers powerful extensions. When comparing, Python internally converts dates to their integer timestamp representation, simplifying comparison.

## See Also (関連情報)
- Python's `datetime` module documentation: https://docs.python.org/3/library/datetime.html
- `dateutil` library on PyPI: https://pypi.org/project/python-dateutil/
- strftime() and strptime() Behavior: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
