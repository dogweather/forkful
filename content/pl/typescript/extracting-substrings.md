---
title:                "Wydobywanie podciągów"
html_title:           "TypeScript: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyłuskiwanie podciągów to powszechnie stosowana technika w programowaniu. Pozwala ona na efektywniejsze manipulowanie danymi, tworzenie nowych ciągów znaków oraz wyciąganie potrzebnych informacji. W artykule tym dowiesz się, dlaczego warto poznać tę technikę i jak jej używać w języku TypeScript.

## Jak To Zrobić

```TypeScript
// Przykładowy ciąg znaków
const text = "Hello World";

// Wyłuskanie podciągu od indeksu 6 do końca ciągu
const substring = text.substring(6); // Wynik: "World"

// Wyłuskanie podciągu od indeksu 4 do indeksu 9
const substring2 = text.substring(4, 9); // Wynik: "o Wor"
```

Powyższe przykłady przedstawiają prosty sposób wyłuskiwania podciągów z ciągu znaków. Metoda `substring()` przyjmuje dwa argumenty: początkowy oraz opcjonalny końcowy indeks podciągu. Jeśli nie podamy drugiego argumentu, zostanie wyłuszczony podciąg od wskazanego indeksu do końca ciągu. Jeśli podamy oba argumenty, zostanie wyłuszczony podciąg od pierwszego do drugiego argumentu (bez włączenia tego drugiego).

## Deep Dive

Metoda `substring()` wykorzystuje indeksy znaków w ciągu, aby wyłuskać podciąg. Indeks pierwszego znaku w ciągu ma wartość 0, a indeks ostatniego znaku to długość ciągu minus 1. Dzięki temu możemy precyzyjnie wyłuskać interesujące nas fragmenty tekstu.

Warto również wspomnieć o metodzie `slice()`, która działa podobnie do `substring()`, jednak przyjmuje również ujemne indeksy, co pozwala na wyłuskiwanie podciągów od końca ciągu.

## Zobacz także

Dowiedz się więcej o wyłuskiwaniu podciągów w języku TypeScript:

- [Dokumentacja MDN: substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Dokumentacja MDN: slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Tutorial na temat manipulowania ciągami znaków w TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)