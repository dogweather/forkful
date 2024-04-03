---
date: 2024-01-20 17:33:31.971848-07:00
description: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra z nich jest wcze\u015B\
  niejsza, p\xF3\u017Aniejsza lub czy s\u0105 identyczne. Programi\u015Bci to robi\u0105\
  , by zarz\u0105dza\u0107 wydarzeniami,\u2026"
lastmod: '2024-03-13T22:44:34.962733-06:00'
model: gpt-4-1106-preview
summary: "Por\xF3wnywanie dw\xF3ch dat to sprawdzanie, kt\xF3ra z nich jest wcze\u015B\
  niejsza, p\xF3\u017Aniejsza lub czy s\u0105 identyczne."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## Jak to zrobić:
```Python
from datetime import datetime

# Przykład dat
data_1 = datetime(2023, 4, 5)
data_2 = datetime(2023, 5, 10)

# Porównanie dat
if data_1 < data_2:
    print("Data 1 jest wcześniejsza niż Data 2")
elif data_1 > data_2:
    print("Data 1 jest późniejsza niż Data 2")
else:
    print("Data 1 i Data 2 są takie same")

# Wynik
# Data 1 jest wcześniejsza niż Data 2
```

## Zanurzenie:
Porównywanie dat to stara praktyka, zaawansowana dzięki komputerom. W Pythonie, standardowa biblioteka `datetime` umożliwia operacje na datach. Alternatywy obejmują biblioteki zewnętrzne jak `dateutil`, która oferuje większą elastyczność. Implementacja porównywania dat opiera się na reprezentacji czasu UNIX - liczbie sekund od północy 1 stycznia 1970 r. Systemy mogą się różnić (np. Windows vs. UNIX), ale w Pythonie to abstrakcyjne dzięki `datetime`.

## Zobacz również:
- Oficjalną dokumentację modułu datetime: https://docs.python.org/3/library/datetime.html
- Dokumentacja dateutil: https://dateutil.readthedocs.io/en/stable/
- UNIX Time: https://en.wikipedia.org/wiki/Unix_time
