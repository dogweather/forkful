---
date: 2024-01-20 17:33:32.346766-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Here's a simple way to compare dates using Python."
lastmod: '2024-04-05T21:53:48.865233-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Here's a simple way to compare dates using Python."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## How to: (Як це зробити:)
Here's a simple way to compare dates using Python:

```Python
from datetime import datetime

# First date in YYYY-MM-DD format
date_string_1 = "2023-04-01"
# Second date in the same format
date_string_2 = "2023-08-24"

# Convert strings to datetime objects
date_1 = datetime.fromisoformat(date_string_1)
date_2 = datetime.fromisoformat(date_string_2)

# Compare the two dates
if date_1 < date_2:
    print(f"{date_string_1} comes before {date_string_2}")
elif date_1 > date_2:
    print(f"{date_string_1} comes after {date_string_2}")
else:
    print(f"{date_string_1} is the same as {date_string_2}")
```

Sample output for the above code:

```
2023-04-01 comes before 2023-08-24
```

## Deep Dive (Поглиблений аналіз):
Long before Python, people compared dates using calendars and calculations. In Python, the `datetime` module became the go-to way to manage dates and times post its introduction in version 2.3.

There are alternatives like using timestamps or third-party libraries (like `dateutil`). Still, `datetime` is robust and built-in, meaning fewer dependencies.

Under the hood, `datetime` objects are compared based on their internal representation of date and time, which makes these operations reliable and fast.

## See Also (Дивіться також):
- `datetime` official docs: https://docs.python.org/3/library/datetime.html
- Python datetime tutorial: https://realpython.com/python-datetime/
- dateutil library docs: https://dateutil.readthedocs.io/en/stable/
