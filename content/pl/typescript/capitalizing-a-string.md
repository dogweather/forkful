---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i Dlaczego?)
Kapitalizacja tekstu to zmiana pierwszej litery w wyrazie na wielką. Programiści używają jej do formatowania tekstów w interfejsach użytkownika czy dokumentach, aby poprawić czytelność i estetykę.

## How to:
(Jak to zrobić:)
```TypeScript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Użycie funkcji:
console.log(capitalize('witaj świecie')); // 'Witaj świecie'
console.log(capitalize('typescript jest fajny')); // 'Typescript jest fajny'
```

## Deep Dive
(Zanurzamy się głębiej)
Kapitalizacja tekstu nie jest niczym nowym; używamy jej w pisowni od wieków. W kontekście programowania, jest szczególnie przydatna w językach interfejsów użytkownika, takich jak HTML i CSS, oraz w praktykach backendowych, np. przy formatowaniu danych przed zapisem do bazy. 

Alternatywy kapitalizacji pierwszej litery obejmują kompletną zamianę na wielkie litery (`toUpperCase()`) albo zmniejszenie ich (`toLowerCase()`). Istnieje też 'title case', gdzie wielką literą zapisujemy każdy wyraz. 

W TypeScript nie ma wbudowanej funkcji do kapitalizacji każdego słowa, więc musimy pisać własne. Sprawdźcie też `localeCompare` dla poprawy porównań stringów w różnych językach i ustawień regionalnych.

## See Also
(Zobacz także)
- MDN Web Docs na temat `toUpperCase()` i `toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- Opis `localeCompare()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare
- CSS Text - Transformacje tekstu: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform