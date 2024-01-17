---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "TypeScript: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwertowanie ciągów znaków na małe litery jest częstym zadaniem dla programistów. Polega to na zamianie wszystkich wielkich liter w ciągu na odpowiadające im małe litery. Robimy to zazwyczaj ze względu na jednolitość lub łatwiejsze porównywanie danych.

## Jak to zrobić:

```typescript
let string = "TEKST DO KONWERTOWANIA";
console.log(string.toLowerCase());
// Output: tekst do konwertowania
```

## Głębsze wyjaśnienia:

1. Konwersja ciągów znaków na małe litery jest powszechnie stosowaną techniką w programowaniu. Początkowo wynikało to z ograniczeń sprzętowych, gdzie używanie tylko jednego rozmiaru liter ułatwiało przechowywanie i porównywanie danych.

2. Alternatywną metodą jest użycie funkcji upperCase(), która zamienia wszystkie litery w ciągu na wielkie. Jednak nie jest to zalecane ze względu na zmianę oryginalnego ciągu.

3. Implementacja konwersji ciągu znaków na małe litery jest różna w zależności od języka programowania. W języku TypeScript możemy użyć funkcji toLowerCase(), która jest wbudowana w obiekt String.

## Zobacz także:

- [Dokumentacja TypeScript o toLowerCase()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-5.html#built-in-operator-o-a-works-on-string)
- [Funkcja String.prototype.toLowerCase() w języku JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)