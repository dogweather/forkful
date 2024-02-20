---
date: 2024-01-20 17:39:04.042949-07:00
description: "Konwersja \u0142a\u0144cucha znak\xF3w do ma\u0142ych liter to proces\
  \ zmiany wszystkich wielkich liter na ma\u0142e. Robimy to dla uproszczenia por\xF3\
  wnywania tekstu,\u2026"
lastmod: 2024-02-19 22:04:54.119988
model: gpt-4-1106-preview
summary: "Konwersja \u0142a\u0144cucha znak\xF3w do ma\u0142ych liter to proces zmiany\
  \ wszystkich wielkich liter na ma\u0142e. Robimy to dla uproszczenia por\xF3wnywania\
  \ tekstu,\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja łańcucha znaków do małych liter to proces zmiany wszystkich wielkich liter na małe. Robimy to dla uproszczenia porównywania tekstu, wyszukiwania, lub by spełnić standardy formatowania danych.

## Jak to zrobić:
```python
# Przykład konwersji tekstu do małych liter
tekst = "Witaj Świecie!"
tekst_male_litery = tekst.lower()

print(tekst_male_litery)  # wyjście: witaj świecie!
```

## Zagłębiamy się
W Pythonie metoda `.lower()` istnieje już od dawna, oferując prosty sposób na konwersję tekstu. Inne języki programowania też mają swoje odpowiedniki. Istnieje wiele alternatyw takich jak `casefold()`, która jest bardziej agresywna i lepiej radzi sobie z niestandardowymi przypadkami, jak np. niemieckie ostre s (ß). Wewnątrz każdego znaku Unicode zapisana jest informacja o tym, jak powinien być przedstawiany w wersji małych liter, co wykorzystywane jest podczas konwersji.

## Zobacz również
- Dokumentacja Python `str.lower()`: https://docs.python.org/3/library/stdtypes.html#str.lower
- Opis metody `str.casefold()`: https://docs.python.org/3/library/stdtypes.html#str.casefold
- Unicode Standard: https://unicode.org/standard/standard.html
