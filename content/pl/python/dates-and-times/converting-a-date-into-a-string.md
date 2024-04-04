---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Jak to zrobi\u0107: Python u\u0142atwia konwersj\u0119 dat na ci\u0105\
  gi znak\xF3w. U\u017Cyj metody [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-\u2026"
lastmod: '2024-04-04T02:02:35.411116-06:00'
model: gpt-4-0125-preview
summary: "Python u\u0142atwia konwersj\u0119 dat na ci\u0105gi znak\xF3w."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## Jak to zrobić:
Python ułatwia konwersję dat na ciągi znaków. Użyj metody [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) dostępnej dla obiektów [date](https://docs.python.org/3/library/datetime.html#date-objects). Oto jak:

```Python
from datetime import datetime

# Pobierz bieżącą datę i czas
now = datetime.now()

# Konwertuj na ciąg znaków w formacie: Miesiąc dzień, Rok
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Wyjście: Marzec 29, 2023 (lub bieżąca data)

# Format: RRRR-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Wyjście: 2023-03-29 (lub bieżąca data)
```


### Jak ja to robię

Tak uzyskuję datę w formacie [ISO 8601](https://www.w3.org/QA/Tips/iso-date) z informacją o strefie czasowej:

```python
def datestamp() -> str:
    """ 
    Bieżąca data i czas ze strefą czasową w formacie ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Przykład wyjścia:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Szczegółowa analiza
Historycznie, konwersja daty na ciąg znaków jest podstawą w programowaniu ze względu na potrzebę reprezentacji dat w formacie czytelnym dla człowieka.

Alternatywami dla `strftime` są używanie metody `isoformat` dla formatu ISO 8601, lub biblioteki stron trzecich takie jak `arrow` i `dateutil`, które oferują bardziej elastyczne opcje parsowania i formatowania.

Pod względem implementacji, `strftime` oznacza "formatowanie ciągu czasu" i ma korzenie w programowaniu w języku C. `strftime` w Pythonie interpretuje kody formatu takie jak `%Y` dla roku i `%m` dla miesiąca, co pozwala na niemal nieskończoną możliwość dostosowywania.

## Zobacz również
Aby zgłębić temat funkcji daty i czasu w Pythonie:
- Oficjalna dokumentacja `datetime` Pythona: https://docs.python.org/3/library/datetime.html
- Dla zainteresowanych kompleksową listą dyrektyw `strftime`: https://strftime.org/
- Aby eksplorować biblioteki stron trzecich dotyczące daty/czasu:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
