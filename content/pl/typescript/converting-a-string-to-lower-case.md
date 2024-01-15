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

## Dlaczego

Konwersja stringów na małe litery może być przydatna, gdy potrzebujemy porównać dwa tekstu bez uwzględnienia wielkości liter lub gdy chcemy uporządkować dane alfabetycznie.

## Jak to zrobić

```TypeScript
let text = "PIĘKNY"; // deklarujemy zmienną z tekstem
let lowercaseText = text.toLowerCase(); // przy użyciu funkcji toLowerCase() konwertujemy tekst na małe litery
console.log(lowercaseText); // wyświetlamy wynik: piękny
```

## Pod lupą

Podczas konwersji, znaki nieznane dla danego języka mogą pozostać bez zmian, np. litery z diakrysimi, znaki specjalne itp. W TypeScript istnieje również funkcja toLocaleLowerCase(), która uwzględnia ustawienia regionalne, co może być przydatne w niektórych przypadkach.

## Zobacz także

- [Mozilla Developer Network - toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript Docs - String](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html#string)
- [Stack Overflow - How to convert a string to lowercase in TypeScript?](https://stackoverflow.com/questions/69691897/how-to-convert-a-string-to-lowercase-in-typescript)