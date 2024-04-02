---
date: 2024-01-20 17:39:22.165294-07:00
description: ''
lastmod: '2024-03-13T22:44:48.847905-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## What & Why?
## Що це таке та навіщо?

Converting a string to lower case means changing all letters in the string to their lower case form. Programmers do this for consistency, such as when comparing user inputs that should be case-insensitive.

## How to:
## Як це зробити:

```TypeScript
let greeting: string = "Привіт, Світе!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "привіт, світе!"
```

## Deep Dive
## Поглиблений огляд:

Historically, case conversion has been used to make text processing uniform, regardless of the case used when the text was inputted. In TypeScript, the `toLowerCase()` method streamlines this process for strings.

Alternatives include manually iterating over each character and transforming it, but that's unnecessary and error-prone when `toLowerCase()` is available. 

Implementation-wise, `toLowerCase()` handles Unicode characters as well, respecting the locality (though, for specific locale rules, `toLocaleLowerCase()` may be used instead).

## See Also
## Додаткова інформація:

- MDN Documentation on `toLowerCase()`: [MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- TypeScript Official Documentation: [TypeScript Language](https://www.typescriptlang.org/docs/)
- Unicode standard for case mapping: [Unicode Case Mapping](https://unicode.org/reports/tr21/tr21-5.html)
