---
date: 2024-01-20 17:35:40.731035-07:00
description: "How to: (Jak to zrobi\u0107:) Historically, JavaScript developers used\
  \ the '+' operator for string concatenation. With the introduction of ES6 (a significant\u2026"
lastmod: '2024-04-05T22:50:49.434138-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historically, JavaScript developers used the '+' operator\
  \ for string concatenation."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## How to: (Jak to zrobić:)
```TypeScript
let greeting: string = "Cześć";
let who: string = "świecie";
let exclamation: string = "!";

// Klasyczny sposób za pomocą operatora '+':
let classicConcatenation = greeting + ", " + who + exclamation;
console.log(classicConcatenation);  // "Cześć, świecie!"

// Template strings (od ES6 czyli TypeScript również):
let templateConcatenation = `${greeting}, ${who}${exclamation}`;
console.log(templateConcatenation);  // "Cześć, świecie!"
```

## Deep Dive (Dogłębna analiza)
Historically, JavaScript developers used the '+' operator for string concatenation. With the introduction of ES6 (a significant update to JavaScript upon which TypeScript is built), template literals—delimited by backticks (`) instead of quotes—came into play, allowing for multiline strings and string interpolation (embedded expressions).

Alternatives to '+' include `concat()` method and array’s `join()` method, though these are less common in modern code:

```TypeScript
// concat() metoda:
let concatMethod = greeting.concat(", ", who, exclamation);
console.log(concatMethod);  // Cześć, świecie!

// join() metoda z użyciem tablicy:
let arrayJoin = [greeting, who].join(", ") + exclamation;
console.log(arrayJoin);  // Cześć, świecie!
```

Under the hood, when you use '+', JavaScript engines can optimize string concatenation efficiently, especially for short and simple operations. However, for complex or large-scale string-building tasks, template literals may offer better performance and readability.

## See Also (Zobacz również)
- [MDN Web Docs on Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
