---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:39:22.165294-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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