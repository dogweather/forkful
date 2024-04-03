---
date: 2024-01-20 17:31:41.674504-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to\
  \ ustalenie, jaka data b\u0119dzie za kilka dni, miesi\u0119cy, lat albo jaka by\u0142\
  a. Programi\u015Bci to robi\u0105, by\u2026"
lastmod: '2024-03-13T22:44:34.963750-06:00'
model: gpt-4-1106-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci to ustalenie,\
  \ jaka data b\u0119dzie za kilka dni, miesi\u0119cy, lat albo jaka by\u0142a."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
