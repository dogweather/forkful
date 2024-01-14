---
title:                "TypeScript: Używanie wyrażeń regularnych"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są powszechnie używane w programowaniu do wyszukiwania i manipulacji ciągami znaków. Są one nie tylko wygodne, ale także bardzo skuteczne w wykonywaniu złożonych operacji na tekstach. W tym artykule dowiesz się, dlaczego warto nauczyć się korzystać z regular expressions w TypeScript.

## Jak to zrobić

Poniżej przedstawiamy kilka przykładów kodu TypeScript wykorzystujących regular expressions, wraz z odpowiadającym im wyjściem.

```typescript
// Wyszukiwanie słów rozpoczynających się od litery "a"
const regex = /a\w+/g;
const text = "Ala ma kota i amorka.";
const matches = text.match(regex);
console.log(matches);
// Output: ['Ala', 'amorka']
```
```typescript
// Zmiana formatu daty z "mm/dd/yyyy" na "dd.mm.yyyy"
const regex = /(\d{2})\/(\d{2})\/(\d{4})/g;
const date = "12/31/2020";
const result = date.replace(regex, `$2.$1.$3`);
console.log(result);
// Output: '31.12.2020'
```
```typescript
// Sprawdzanie czy tekst zawiera liczby
const regex = /\d/;
const text = "Nie spodziewam się, że w tym zdaniu będzie 200.";
console.log(regex.test(text));
// Output: true
```

## Deep Dive

Regular expressions w TypeScript dają możliwość wykonywania zaawansowanych operacji na tekstach, dzięki czemu nasze aplikacje mogą być bardziej funkcjonalne i wydajne. Nie tylko są one używane do wyszukiwania, ale także do zastępowania, dzielenia i walidacji ciągów znaków. Poza tym, TypeScript dostarcza specjalne notacje, które ułatwiają pracę z regular expressions, takie jak `\w` oznaczający dowolny znak alfanumeryczny lub `\d`, który oznacza dowolną cyfrę.

Gdy jesteś już gotowy, aby przyswoić wiedzę na temat regular expressions w TypeScript, możesz poszerzyć swoje umiejętności, wykorzystując język regex w innych językach programowania, takich jak Python czy JavaScript.

## Zobacz także

- [Dokumentacja TypeScript dotycząca regular expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Książka "JavaScript - Rola wyrażeń regularnych w praktyce"](https://helion.pl/ksiazki/javascript-rola-wyrazen-regularnych-w-praktyce-srp-jsre.htm#format/e)
- [Wideo "Wyrażenia regularne w TypeScript"](https://www.youtube.com/watch?v=JdDXKf_5dfk)