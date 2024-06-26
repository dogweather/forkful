---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:30.045190-07:00
description: "Jak to zrobi\u0107: Standardowa biblioteka Pythona dostarcza modu\u0142\
  \ `datetime`, kt\xF3ry zawiera metod\u0119 `strptime` przeznaczon\u0105 do tego\
  \ celu. Metoda ta wymaga\u2026"
lastmod: '2024-03-13T22:44:34.959376-06:00'
model: gpt-4-0125-preview
summary: "Standardowa biblioteka Pythona dostarcza modu\u0142 `datetime`, kt\xF3ry\
  \ zawiera metod\u0119 `strptime` przeznaczon\u0105 do tego celu."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
Standardowa biblioteka Pythona dostarcza moduł `datetime`, który zawiera metodę `strptime` przeznaczoną do tego celu. Metoda ta wymaga dwóch argumentów: ciągu znaków z datą oraz dyrektywy formatującej, która określa wzór ciągu wejściowego.

```python
from datetime import datetime

# Przykładowy ciąg znaków
data_string = "2023-04-01 14:30:00"
# Przetwarzanie ciągu znaków na obiekt datetime
przetworzona_data = datetime.strptime(data_string, "%Y-%m-%d %H:%M:%S")

print(przetworzona_data)
# Wyjście: 2023-04-01 14:30:00
```

Dla bardziej zniuansowanego przetwarzania dat, szczególnie przy obchodzeniu się z wieloma formatami lub ustawieniami lokalnymi, bardzo przydatna może być biblioteka stron trzecich `dateutil`. Dostarcza ona moduł analizatora, który potrafi przetwarzać daty w niemal każdym formacie ciągu znaków.

```python
from dateutil import parser

# Przykładowe ciągi znaków
data_string1 = "April 1, 2023 2:30 PM"
data_string2 = "1st April 2023 14:30"

# Użycie analizatora z dateutil
przetworzona_data1 = parser.parse(data_string1)
przetworzona_data2 = parser.parse(data_string2)

print(przetworzona_data1)
# Wyjście: 2023-04-01 14:30:00
print(przetworzona_data2)
# Wyjście: 2023-04-01 14:30:00
```

`dateutil` jest biegły w obsłudze większości formatów dat bez potrzeby jawnego określania łańcuchów formatujących, co czyni go uniwersalnym wyborem dla aplikacji radzących sobie z różnorodnymi reprezentacjami dat.
