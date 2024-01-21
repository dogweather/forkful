---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:58.889866-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Concatenating strings means sticking them together end-to-end to make one longer string. Programmers do this to combine text in ways that make sense for the task, like creating messages or building URLs.

## How to: (Як це зробити:)

```typescript
// Using the + operator
let greeting: string = "Привіт, " + "світе!";
console.log(greeting); // "Привіт, світе!"

// Using template literals (backticks)
let user: string = "Андрій";
let age: number = 25;
let userInfo: string = `Ім'я користувача: ${user}, вік: ${age}`;
console.log(userInfo); // "Ім'я користувача: Андрій, вік: 25"
```

## Deep Dive (Поглиблений Занурення)

In the early days, concatenating strings was often tedious, especially in languages without built-in support. JavaScript, and therefore TypeScript, has always made this easy with the + operator. However, since ES2015, template literals have been the trend because they’re more readable and offer direct interpolation of variables and expressions.

Alternatives to string concatenation include the `String.prototype.concat()` method, but it's less common due to verbosity. For large-scale string manipulation (like generating HTML or creating long reports), you might also consider string builders or buffers, but TypeScript developers typically use simple concatenation or template literals.

As for implementation, remember that strings in TypeScript are immutable. Every concatenation creates a new string instead of modifying an existing one, which can matter for performance in heavy string processing tasks.

## See Also (Дивіться також)

- [MDN Web Docs: Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [w3schools TypeScript Tutorial](https://www.w3schools.com/typescript/)