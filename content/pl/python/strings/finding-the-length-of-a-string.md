---
date: 2024-01-20 17:48:11.871397-07:00
description: "Zliczanie znak\xF3w w ci\u0105gu znak\xF3w to podstawa. Programi\u015B\
  ci robi\u0105 to, by wiedzie\u0107, ile danych maj\u0105 przed sob\u0105 \u2013\
  \ czy to waliduj\u0105c input, formatuj\u0105c tekst, czy\u2026"
lastmod: '2024-02-25T18:49:33.365532-07:00'
model: gpt-4-1106-preview
summary: "Zliczanie znak\xF3w w ci\u0105gu znak\xF3w to podstawa. Programi\u015Bci\
  \ robi\u0105 to, by wiedzie\u0107, ile danych maj\u0105 przed sob\u0105 \u2013 czy\
  \ to waliduj\u0105c input, formatuj\u0105c tekst, czy\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zliczanie znaków w ciągu znaków to podstawa. Programiści robią to, by wiedzieć, ile danych mają przed sobą – czy to walidując input, formatując tekst, czy sprawdzając na potrzeby logiki programu.

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
