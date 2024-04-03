---
date: 2024-01-20 17:37:33.589899-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-03-13T22:44:34.961499-06:00'
model: gpt-4-1106-preview
summary: .
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## Jak to zrobić:
```python
from datetime import datetime

# Aktualna data i czas
teraz = datetime.now()

# Konwersja na napis w standardowym formacie
napis1 = teraz.strftime("%Y-%m-%d %H:%M:%S")
print(napis1)  # Wyświetli np. '2023-04-02 15:45:32'

# Konwersja na napis w niestandardowym formacie
napis2 = teraz.strftime("%d/%m/%Y")
print(napis2)  # Wyświetli np. '02/04/2023'
```

## Deep Dive
Konwersja daty na napis ma długa historię, gdzie tradycyjnie używano funkcji pokroju `sprintf` w językach takich jak C. W Pythonie, metoda `strftime` pozwala na elastyczną konwersję zgodnie z zadanym wzorcem. Alternatywą jest użycie biblioteki `arrow` lub `dateutil`, które oferują dodatkowe formy manipulacji datami i mogą być przydatne, jeśli potrzebujemy obsługi stref czasowych lub lokalizacji daty.

Formatowanie daty implementowane jest przez obiekty typu datetime. Zawierają one metody, jak `strftime`, która przyjmuje łańcuch formatujący składający się z dyrektyw rozpoczynających się od znaku `%`, na przykład `%Y` dla pełnego roku, `%m` dla miesiąca i `%d` dla dnia.

## Zobacz także
- Dokumentacja metody `strftime`: https://docs.python.org/3/library/datetime.html#datetime.date.strftime
- Biblioteka `arrow`: https://arrow.readthedocs.io/
- Biblioteka `dateutil`: https://dateutil.readthedocs.io/
- Wzorce formatowania dla `strftime`/`strptime`: https://strftime.org/
