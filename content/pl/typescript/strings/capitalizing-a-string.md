---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:56.043300-07:00
description: "Jak to zrobi\u0107: TypeScript, b\u0119d\u0105c nadzbiorem JavaScript,\
  \ umo\u017Cliwia r\xF3\u017Cne metody kapitalizacji \u0142a\u0144cuch\xF3w, od czystych\
  \ podej\u015B\u0107 w JavaScript po wykorzystanie\u2026"
lastmod: '2024-03-13T22:44:35.120983-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, b\u0119d\u0105c nadzbiorem JavaScript, umo\u017Cliwia r\xF3\u017C\
  ne metody kapitalizacji \u0142a\u0144cuch\xF3w, od czystych podej\u015B\u0107 w\
  \ JavaScript po wykorzystanie bibliotek stron trzecich do bardziej z\u0142o\u017C\
  onych lub specyficznych przypadk\xF3w u\u017Cycia."
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
