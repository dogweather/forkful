---
title:                "Porównywanie dwóch dat"
date:                  2024-01-20T17:33:31.971848-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Porównywanie dwóch dat to sprawdzanie, która z nich jest wcześniejsza, późniejsza lub czy są identyczne. Programiści to robią, by zarządzać wydarzeniami, wygasaniem certyfikatów czy datami ważności.

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