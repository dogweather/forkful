---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Jak to zrobi\u0107: ."
lastmod: '2024-04-04T01:27:53.333215-06:00'
model: gpt-4-0125-preview
summary: .
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Jak to zrobić:
```Python
import re

# Przykładowy ciąg znaków
text = "Hello, World! 1234"

# Usunięcie wszystkich cyfr
bez_cyfr = re.sub(r'\d', '', text)
print(bez_cyfr)  # Wynik: "Hello, World! "

# Usunięcie znaków interpunkcyjnych
bez_interpunkcji = re.sub(r'[^\w\s]', '', text)
print(bez_interpunkcji)  # Wynik: "Hello World 1234"

# Usunięcie samogłosek
bez_samoglosek = re.sub(r'[aeiouAEIOU]', '', text)
print(bez_samoglosek)  # Wynik: "Hll, Wrld! 1234"
```

### Funkcja, którą napisałem

Ponieważ robię to dość często, zrefaktoryzowałem to do funkcji `delete()`. Jest to również dobra demonstracja [doctestów](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```

## Dogłębne spojrzenie
Praktyka usuwania znaków pasujących do wzorca w tekście ma głębokie korzenie w informatyce, sięgając wczesnych narzędzi Unix, takich jak `sed` i `grep`. W Pythonie, moduł `re` zapewnia tę możliwość, wykorzystując wyrażenia regularne - potężne i wszechstronne narzędzie do przetwarzania tekstu.

Alternatywy dla modułu `re` to między innymi:
- Metody ciągu znaków, takie jak `replace()`, dla prostych przypadków.
- Biblioteki innych firm, takie jak `regex`, dla bardziej skomplikowanych wzorców i lepszego wsparcia dla Unicode.

Pod maską, gdy używasz `re.sub()`, interpreter Pythona kompiluje wzorzec do serii bajtkodów, przetwarzanych przez maszynę stanów, która wykonuje dopasowanie wzorca bezpośrednio na wejściowym tekście. Operacja ta może być zasobożerna dla dużych ciągów znaków lub skomplikowanych wzorców, dlatego rozważenia dotyczące wydajności są kluczowe przy przetwarzaniu dużych danych.

## Zobacz również
- [Dokumentacja modułu `re` w Pythonie](https://docs.python.org/3/library/re.html): Oficjalna dokumentacja dla wyrażeń regularnych w Pythonie.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Szczegółowy przewodnik po wyrażeniach regularnych.
- [Samouczek Real Python na temat regex](https://realpython.com/regex-python/): Praktyczne zastosowania wyrażeń regularnych w Pythonie.
