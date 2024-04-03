---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:56.043300-07:00
description: "Kapitalizacja \u0142a\u0144cucha znak\xF3w polega na zmodyfikowaniu\
  \ pierwszego znaku danego ci\u0105gu na du\u017C\u0105 liter\u0119, je\u015Bli jest\
  \ on w ma\u0142ej literze, cz\u0119sto pozostawiaj\u0105c\u2026"
lastmod: '2024-03-13T22:44:35.120983-06:00'
model: gpt-4-0125-preview
summary: "Kapitalizacja \u0142a\u0144cucha znak\xF3w polega na zmodyfikowaniu pierwszego\
  \ znaku danego ci\u0105gu na du\u017C\u0105 liter\u0119, je\u015Bli jest on w ma\u0142\
  ej literze, cz\u0119sto pozostawiaj\u0105c reszt\u0119 ci\u0105gu bez zmian."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
TypeScript, będąc nadzbiorem JavaScript, umożliwia różne metody kapitalizacji łańcuchów, od czystych podejść w JavaScript po wykorzystanie bibliotek stron trzecich do bardziej złożonych lub specyficznych przypadków użycia.

**Podejście czystego JavaScript:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Przykładowe wyjście:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Ta metoda jest prosta i opiera się na metodzie `charAt()`, aby uzyskać dostęp do pierwszego znaku ciągu oraz `toUpperCase()`, aby przekształcić go na dużą literę. Metoda `slice(1)` następnie pobiera resztę ciągu, pozostawiając ją bez zmian.

**Korzystanie z biblioteki Lodash:**

Dla projektów już wykorzystujących bibliotekę [Lodash](https://lodash.com/), można wykorzystać jej funkcję `_.capitalize` do osiągnięcia tego samego wyniku z mniejszą ilością kodu.

Najpierw zainstaluj Lodash:

```bash
npm install lodash
```

Następnie użyj go w swoim pliku TypeScript:

```typescript
import * as _ from 'lodash';

// Przykładowe wyjście:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Uwaga: Metoda `_.capitalize` biblioteki Lodash konwertuje resztę ciągu na małe litery, co może nie zawsze być pożądanym efektem.

**Korzystanie z wyrażenia regularnego:**

Wyrażenie regularne może zapewnić zwięzły sposób na kapitalizację pierwszej litery ciągu, szczególnie jeśli potrzebujesz kapitalizować pierwszą literę każdego słowa w ciągu.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Przykładowe wyjście:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Ta metoda wykorzystuje funkcję `replace()` do wyszukiwania dowolnego granicznika słowa po którym następuje alfanumeryczny znak (`\b\w`), kapitalizując każde dopasowanie. Jest szczególnie przydatna dla tytułów lub nagłówków.
