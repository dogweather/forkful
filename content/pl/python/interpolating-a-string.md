---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:51:23.294723-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja napisów to wplecenie zmiennych czy też wartości bezpośrednio do ciągu tekstowego. Programiści używają tego, żeby ułatwić formatowanie i składanie komunikatów, zwłaszcza gdy dynamika danych jest na pierwszym planie.

## Jak to zrobić:

```Python
# Przykład użycia f-string do interpolacji napisów.
imie = "Anna"
wiek = 30

# Tradycyjnie z użyciem .format()
wypowiedz_format = "Nazywam się {} i mam {} lat.".format(imie, wiek)
print(wypowiedz_format)
# Wynik: Nazywam się Anna i mam 30 lat.

# Nowocześniej: f-string
wypowiedz_fstring = f"Nazywam się {imie} i mam {wiek} lat."
print(wypowiedz_fstring)
# Wynik: Nazywam się Anna i mam 30 lat.
```

## Głębsze spojrzenie

Interpolacja napisów istnieje w Pythonie od dawna – najpierw były to operator `%`, później metoda `.format()`, a od Pythona 3.6 mamy f-stringi. F-stringi są szybsze niż poprzednie metody i pozwalają na bezpośrednie wstawianie wyrażeń Pythona w nawiasach klamrowych. Za kulisami Python przetwarza taki napis na wyrażenie formatujące i wykonuje je w locie.

Innymi sposobami interpolacji są template strings (`string.Template`), które mogą być użyteczne dla prostych przypadków i kiedy chcesz unikać ewaluacji wyrażeń wewnątrz napisów.

## Zobacz również

- [Standardowa dokumentacja Pythona o f-stringach](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [PEP 498 – wprowadzenie f-stringów](https://www.python.org/dev/peps/pep-0498/)
- [Python Software Foundation: PyFormat](https://pyformat.info/) – Porównanie metod formatowania napisów w Pythonie.