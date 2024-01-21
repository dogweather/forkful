---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:31:41.674504-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obliczanie daty w przyszłości lub przeszłości to ustalenie, jaka data będzie za kilka dni, miesięcy, lat albo jaka była. Programiści to robią, by zarządzać wydarzeniami, terminami, harmonogramami.

## Jak to zrobić:
```Python
from datetime import datetime, timedelta

# Aktualna data i czas
now = datetime.now()
print(f"Teraz: {now}")

# Obliczanie daty w przyszłości (za 10 dni)
future_date = now + timedelta(days=10)
print(f"Data za 10 dni: {future_date}")

# Obliczanie daty w przeszłości (10 dni temu)
past_date = now - timedelta(days=10)
print(f"Data 10 dni temu: {past_date}")
```
Wyjście:
```
Teraz: 2023-04-05 14:23:01.123456
Data za 10 dni: 2023-04-15 14:23:01.123456
Data 10 dni temu: 2023-03-26 14:23:01.123456
```

## Pogłębione informacje:
Moduł `datetime` w Pythonie istnieje od dawna i jest jednym z głównych sposobów na manipulację datami i czasem. Alternatywy to m.in. `dateutil`, `arrow`, czy `pendulum`, które mogą oferować dodatkową funkcjonalność. Techniczne zaplecze obliczeń dat opiera się na rozumieniu epoki (czas od określonej daty, na ogół od 1 stycznia 1970 r.) i reprezentacji czasu w sekundach od tej epoki.

## Zobacz także:
- Dokumentacja Python `datetime`: https://docs.python.org/3/library/datetime.html
- PyPI Dateutil: https://pypi.org/project/python-dateutil/
- PyPI Arrow: https://pypi.org/project/arrow/
- PyPI Pendulum: https://pypi.org/project/pendulum/