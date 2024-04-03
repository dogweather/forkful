---
date: 2024-01-20 17:38:59.319736-07:00
description: "Konwersja \u0142a\u0144cucha znak\xF3w (string) do ma\u0142ych liter\
  \ to proces zmiany wszystkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki.\
  \ Robimy to, aby ujednolici\u0107 dane,\u2026"
lastmod: '2024-03-13T22:44:35.783562-06:00'
model: gpt-4-1106-preview
summary: "Konwersja \u0142a\u0144cucha znak\xF3w (string) do ma\u0142ych liter to\
  \ proces zmiany wszystkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## Jak to zrobić:
```Javascript
let greeting = "Witaj Świecie!";
let lowerCaseGreeting = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "witaj świecie!"
```

```Javascript
// Przykład z użyciem łańcuchów znaków zawierających polskie znaki diakrytyczne
let polishText = "Język JavaScript jest Zabawny!";
let lowerCasePolishText = polishText.toLowerCase();

console.log(lowerCasePolishText); // "język javascript jest zabawny!"
```

## W głębi tematu
Konwersja tekstów do małych liter jest standardowym narzędziem w programowaniu od lat. W JavaScript funkcja `toLowerCase()` istnieje od początku i jest częścią standardu ECMAScript. Alternatywą jest funkcja `toLocaleLowerCase()`, która uwzględnia specyfikę lokalną – na przykład tureckie i azerskie małe i duże litery 'i'. Szczegóły implementacji różnią się w zależności od środowiska, ale ogólnie `toLowerCase()` działa poprzez mapowanie każdej dużej litery do jej małej odpowiedniczki w utf-16.

## Zobacz również
- MDN Web Docs o `toLowerCase()`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- MDN Web Docs o `toLocaleLowerCase()`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- Specyfikacja ECMAScript dla `String.prototype.toLowerCase()`: [https://tc39.es/ecma262/#sec-string.prototype.tolowercase](https://tc39.es/ecma262/#sec-string.prototype.tolowercase)
