---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Python: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego?

Liczenie daty w przyszłości lub przeszłości może być przydatne w różnych sytuacjach, na przykład w planowaniu wydarzeń, określaniu ważnych dni czy w obliczaniu wieku.

## Jak to zrobić?

W celu obliczenia daty w przyszłości lub przeszłości w języku Python, można skorzystać z modułu `datetime`. Najpierw należy zaimportować ten moduł, a następnie użyć funkcji `timedelta`, która pozwala na ustawienie dowolnej liczby dni, tygodni, miesięcy lub lat.

Przykłady:

```
from datetime import datetime, timedelta

# Obliczanie daty 30 dni od teraz
now = datetime.now()
future = now + timedelta(days=30)
print(future)

# Obliczanie daty 2 tygodnie w przeszłości
past = now - timedelta(weeks=2)
print(past)
```

Wynik:

```
2021-11-19 14:00:00.399843
2021-10-05 14:00:00.399843
```

## Warto wiedzieć!

Funkcja `datetime.now()` zwraca aktualną datę i czas, a `timedelta()` pozwala na ustawienie dowolnych wartości w celu zwiększenia lub zmniejszenia daty. Jest to przydatne w przypadku potrzeby obliczenia daty w przyszłości lub przeszłości na podstawie innych, już istniejących dat.

Można również użyć operatora arytmetycznego `-` w przypadku odejmowania dat, co pozwala na określenie różnicy między datami.

## Zobacz również

- Dokumentacja modułu datetime: https://docs.python.org/3/library/datetime.html
- Poradnik programowania w Pythonie: https://pythonguide.pl/