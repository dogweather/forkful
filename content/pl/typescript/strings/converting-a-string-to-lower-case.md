---
date: 2024-01-20 17:39:30.330469-07:00
description: "Zmiana ci\u0105gu znak\xF3w na ma\u0142e litery to przekszta\u0142cenie\
  \ wszystkich liter w ci\u0105gu na ich odpowiedniki w dolnym rejestrze. Robimy to,\
  \ by ujednolici\u0107 dane\u2026"
lastmod: '2024-03-13T22:44:35.125325-06:00'
model: gpt-4-1106-preview
summary: "Zmiana ci\u0105gu znak\xF3w na ma\u0142e litery to przekszta\u0142cenie\
  \ wszystkich liter w ci\u0105gu na ich odpowiedniki w dolnym rejestrze."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## What & Why? (Co i dlaczego?)
Zmiana ciągu znaków na małe litery to przekształcenie wszystkich liter w ciągu na ich odpowiedniki w dolnym rejestrze. Robimy to, by ujednolicić dane przed ich przetworzeniem, co ułatwia porównywanie i wyszukiwanie.

## How to (Jak to zrobić):
Użyj metody `.toLowerCase()` na ciągu znaków w TypeScript, jak w przykładzie poniżej.

```typescript
let greeting: string = "Witaj Świecie!";
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "witaj świecie!"
```

Sample output (Przykładowe wyjście):
```
witaj świecie!
```

## Deep Dive (Dogłębna analiza):
JavaScript (a tym samym TypeScript) dostarcza metodę `.toLowerCase()` od wczesnych wersji, będącą częścią standardu ECMAScript. Ta metoda zwraca nowy łańcuch znaków ze wszystkimi literami zamienionymi na małe litery, nie zmieniając oryginalnego ciągu.

Alternatywą jest użycie wyrażeń regularnych i metody `.replace()` do własnoręcznego zamieniania liter, choć w praktyce jest to rzadziej używane:

```typescript
let headline: string = "TypeScript jest SUPER!";
let customLowerCaseHeadline: string = headline.replace(/[A-Z]/g, char => char.toLowerCase());

console.log(customLowerCaseHeadline); // "typescript jest super!"
```

Co do implementacji, `.toLowerCase()` bierze pod uwagę lokalne ustawienia, np. dla liter w języku tureckim, gdzie zamiana `I` na `i` jest inna niż w większości języków.

## See Also (Zobacz także):
- MDN Web Docs: [.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- TypeScript Official Handbook: [Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- ECMAScript Language Specification: [String.prototype.toLowerCase()](https://tc39.es/ecma262/#sec-string.prototype.tolowercase)
