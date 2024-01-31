---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Co to jest i dlaczego?"

Kapitalizacja to proces zmiany pierwszej litery wyrazu na dużą. Programiści używają kapitalizacji, by poprawić czytelność tekstu lub wyznaczyć standard formatowania, np. w nazwach tytułów czy własnych.

## How to:
"Jak to zrobić:"

```python
# Kapitalizacja pojedynczego słowa
word = "python"
capitalized_word = word.capitalize()
print(capitalized_word) # Wyjście: Python

# Kapitalizacja każdego wyrazu w stringu
title = "przykład kapitalizacji tytułu"
capitalized_title = title.title()
print(capitalized_title) # Wyjście: Przykład Kapitalizacji Tytułu
```

## Deep Dive
"Dogłębna analiza"

Kapitalizacja w informatyce nie zawsze była standardem, ale stała się popularna wraz z potrzebą lepszego dostosowywania tekstu dla użytkownika. Inne metody to np. `upper()` (wszystkie litery duże) czy `lower()` (wszystkie litery małe). Implementacja kapitalizacji zależy od języka programowania i użytego zestawu znaków (np. ASCII, Unicode). Python, korzystając z Unicode, pozwala na kapitalizację znaków z różnych alfabetów, nie tylko łacińskiego.

## See Also
"Zobacz także"

- Dokumentacja Pythona o metodach stringów: https://docs.python.org/3/library/stdtypes.html#string-methods
- Unicode Standard: https://www.unicode.org/standard/standard.html
- PEP 8 – Style Guide for Python Code, który opisuje konwencje nazewnictwa: https://www.python.org/dev/peps/pep-0008/
