---
title:                "Interpolacja ciągu znaków"
html_title:           "Python: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest interpolacja ciągu znaków i dlaczego programiści to robią?

Interpolacja ciągu znaków to proces wstawiania wartości zmiennych do wstępnie zdefiniowanego ciągu tekstowego. Programiści wykorzystują to narzędzie, aby upraszczać swoją pracę przy tworzeniu dynamicznych i bardziej złożonych ciągów tekstowych.

## Jak to zrobić:

```python
Imie = "Maria"
Wiek = 25
print("Cześć, mam na imię {0} i mam {1} lat!".format(Imie, Wiek))
```
**Wynik:**
*Cześć, mam na imię Maria i mam 25 lat!*

## Głębszy wgląd:

1. Kontekst historyczny: Interpolacja ciągu znaków została wprowadzona w języku programowania C w latach 70. XX wieku, a od tego czasu zyskała popularność w innych językach.
2. Alternatywy: Inne sposoby tworzenia dynamicznych ciągów tekstowych to użycie operatora `%` lub f-stringów (dostępnych w wersji Python 3.6 i nowszych).
3. Szczegóły implementacji: W Pythonie interpolacja ciągu znaków jest realizowana za pomocą metody `format()` lub operatora `%`. W obu przypadkach wykorzystywane są znaki specjalne (`{}` lub `%s`) do oznaczania miejsc, w których zostaną wstawione wartości zmiennych.

## Zobacz również:

[PEP 3101 - Advanced String Formatting](https://www.python.org/dev/peps/pep-3101/) - oficjalna dokumentacja interpolacji ciągu znaków w Pythonie.
[Real Python - String Interpolation in Python](https://realpython.com/python-string-formatting/) - artykuł o różnych sposobach formatowania ciągów tekstowych w Pythonie.