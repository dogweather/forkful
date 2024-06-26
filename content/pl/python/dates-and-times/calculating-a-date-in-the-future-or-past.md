---
date: 2024-01-20 17:31:41.674504-07:00
description: "Jak to zrobi\u0107: Modu\u0142 `datetime` w Pythonie istnieje od dawna\
  \ i jest jednym z g\u0142\xF3wnych sposob\xF3w na manipulacj\u0119 datami i czasem.\
  \ Alternatywy to m.in.\u2026"
lastmod: '2024-04-05T22:50:49.271925-06:00'
model: gpt-4-1106-preview
summary: "Modu\u0142 `datetime` w Pythonie istnieje od dawna i jest jednym z g\u0142\
  \xF3wnych sposob\xF3w na manipulacj\u0119 datami i czasem."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
