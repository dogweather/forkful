---
title:                "Konwertowanie daty na ciąg znaków."
html_title:           "Python: Konwertowanie daty na ciąg znaków."
simple_title:         "Konwertowanie daty na ciąg znaków."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konwersja daty na string w programowaniu oznacza przekształcenie daty (wyrażonej w różnych formatach lub obiektach) na tekstową reprezentację daty. Programiści mogą tego dokonywać z różnych powodów, na przykład dla przekazania daty wewnątrz plików, wyświetlenia jej użytkownikowi lub przetwarzania danych.

## Jak to zrobić:
### Przykład 1:
```Python
import datetime
today = datetime.date.today()
print("Dzisiaj jest", str(today))
```
Output: "Dzisiaj jest 2021-05-31"

### Przykład 2:
```Python
import datetime
date = datetime.date(2021, 5, 31)
print("Data urodzenia:", str(date))
```
Output: "Data urodzenia: 2021-05-31"

### Przykład 3:
```Python
import datetime
date = "31/05/2021"
print("Dzisiaj jest", datetime.datetime.strptime(date, "%d/%m/%Y").date())
```
Output: "Dzisiaj jest 2021-05-31"

## Głębszy zanurzenie:
1. Istnieje wiele formatów daty w różnych językach programowania, na przykład daty w Pythonie są przechowywane jako obiekty datetime, natomiast w JavaScript jako liczby reprezentujące ilość milisekund od 1 stycznia 1970.
2. Alternatywą dla konwersji daty na string jest przechowywanie daty jako obiektu datetime i używanie odpowiednich funkcji do przetwarzania i wyświetlania daty do użytkownika.
3. W Pythonie dostępna jest funkcja strftime(), która pozwala na konwersję daty na string z wykorzystaniem różnych formatów.

## Zobacz też:
- [Dokumentacja Pythona na temat konwersji daty na string](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
- [Artykuł na temat przechowywania dat w JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)