---
date: 2024-01-20 17:39:04.042949-07:00
description: "Jak to zrobi\u0107: W Pythonie metoda `.lower()` istnieje ju\u017C od\
  \ dawna, oferuj\u0105c prosty spos\xF3b na konwersj\u0119 tekstu. Inne j\u0119zyki\
  \ programowania te\u017C maj\u0105 swoje\u2026"
lastmod: '2024-04-05T21:53:36.384964-06:00'
model: gpt-4-1106-preview
summary: "W Pythonie metoda `.lower()` istnieje ju\u017C od dawna, oferuj\u0105c prosty\
  \ spos\xF3b na konwersj\u0119 tekstu."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

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
