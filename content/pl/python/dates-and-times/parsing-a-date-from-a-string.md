---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases: - /pl/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:30.045190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie daty z ciągu znaków polega na konwersji tekstowej informacji o dacie i czasie na obiekt datetime lub równoważny, uporządkowany format. Czynność ta jest powszechnie wykonywana, aby umożliwić operacje na datach, takie jak arytmetyka, porównania i formatowanie, w sposób niezależny od języka i regionu. Programiści robią to, aby efektywnie obsługiwać i manipulować danymi czasowymi pozyskanymi z logów, danych wprowadzonych przez użytkowników lub źródeł zewnętrznych.

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
