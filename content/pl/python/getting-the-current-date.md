---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:08.887424-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest i dlaczego?

W programowaniu często potrzebujemy aktualnej daty, aby zarządzać wydarzeniami, logować zdarzenia czy ustawiać ważność danych. To tak, jakby mieć zawsze pod ręką kalendarz - niezbędny do organizacji.

## How to:
Jak to zrobić:

Python oferuje moduł `datetime` do łatwego zarządzania datami i czasem. Oto jak szybko uzyskać dzisiejszą datę:

```python
from datetime import date

dzisiejsza_data = date.today()
print(f"Dzisiejsza data to: {dzisiejsza_data}")
```

Sample output:
```
Dzisiejsza data to: 2023-03-16
```

Możesz też dostać datę i czas razem:

```python
from datetime import datetime

obecny_moment = datetime.now()
print(f"Obecny czas to: {obecny_moment}")
```

Sample output:
```
Obecny czas to: 2023-03-16 15:42:08.582030
```

## Deep Dive:
Pogłębione informacje:

Moduł `datetime` w Pythonie pochodzi z wczesnych lat języka, jednak jest regularnie aktualizowany. Daje on wiele możliwości - od prostych (jak powyżej) do skomplikowanych (np. operacje z strefami czasowymi).

Alternatywowym rozwiazaniem jest użycie modułów zewnętrznych jak `pendulum` lub `arrow`, które oferują dodatkową funkcjonalność i prostszą obsługę stref czasowych.

Szczegół implementacji `datetime` obejmuje `C`-type struktury do efektywnego zarządzania czasem, co jest istotne w przypadkach gdzie wydajność ma kluczowe znaczenie.

## See Also:
Zobacz także:

- [Python's datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [PyPI page for Pendulum](https://pypi.org/project/pendulum/)
- [PyPI page for Arrow](https://pypi.org/project/arrow/)
