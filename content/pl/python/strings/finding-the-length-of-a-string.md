---
date: 2024-01-20 17:48:11.871397-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-04-05T21:53:36.388869-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## How to: (Jak to zrobić:)
```Python
# Zliczanie znaków w Pythonie
tekst = "Witaj, Świecie!"
dlugosc = len(tekst)

# Wyświetl długość tekstu
print(dlugosc)  # Wyjście: 15
```

## Deep Dive (Dogłębna Analiza)
Funkcja `len()` w Pythonie wykorzystuje metodę `__len__()` zaimplementowaną przez różne typy danych. Nie jest to tylko liczba znaków – dla tekstów Unicode wyraża liczbę punktów kodowych, co może być inne niż oczekiwane w przypadku znaków współdzielonych. Alternatywnie, możesz użyć pętli `for` by samemu policzyć znaki:

```Python
dlugosc = 0
for znak in tekst:
    dlugosc += 1
```

Ale dlaczego się męczyć, skoro `len()` to robi za nas? Poza tym, `len()` jest szybkie – wykonuje się w czasie stałym, znanym jako czas O(1), bo długość przechowywana jest razem z ciągiem znaków.

## See Also (Zobacz Również)
- Oficjalna dokumentacja Pythona na temat wbudowanych typów i funkcji: [Dokumentacja Pythona](https://docs.python.org/3/library/stdtypes.html)
- Artykuł o Unicode i problemach z długością tekstu: [JoelonSoftware](https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/)
