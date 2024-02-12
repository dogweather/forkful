---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases:
- pl/typescript/capitalizing-a-string.md
date:                  2024-02-03T19:06:56.043300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja łańcucha znaków polega na zmodyfikowaniu pierwszego znaku danego ciągu na dużą literę, jeśli jest on w małej literze, często pozostawiając resztę ciągu bez zmian. Operacja ta jest zwykle używana, aby zapewnić, że nazwy własne lub początki zdań przestrzegają reguł gramatycznych w przetwarzaniu tekstu, co sprawia, że wyniki wyglądają profesjonalnie i są czytelne.

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
