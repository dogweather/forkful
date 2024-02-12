---
title:                "Інтерполяція рядків"
aliases:
- /uk/typescript/interpolating-a-string.md
date:                  2024-01-20T17:52:37.287113-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(Що і Чому?)

String interpolation allows embedding expressions within string literals. Programmers use it to concatenate strings and expressions neatly and readably.

(Інтерполяція рядків дозволяє вбудовувати вирази всередину строкових літералів. Програмісти використовують її для зручного і читабельного об'єднання рядків та виразів.)

## How to:
(Як це зробити:)

```TypeScript
let userName: string = 'Viktor';
let itemsPurchased: number = 3;
let purchaseMessage: string = `Hello, ${userName}! You've purchased ${itemsPurchased} items.`;

console.log(purchaseMessage);
```

Output will be:

(Вивід буде наступним:)

```
Hello, Viktor! You've purchased 3 items.
```

## Deep Dive
(Поглиблений Огляд)

Before ES6, string concatenation relied on the '+' operator, often leading to clunky code. String interpolation in TypeScript, as in modern JavaScript, uses backticks (`` ` ``) and `${expression}` syntax to simplify this process. Beyond the basic `${variable}`, you can run any expression, like `${1 + 1}` or `${fullName.toUpperCase()}`. This interpolation is evaluated at runtime, meaning your expressions are calculated when the code runs.

(До ES6 для конкатенації рядків застосовувався оператор '+', що часто призводило до незручного коду. Інтерполяція рядків у TypeScript, як і в сучасному JavaScript, використовує зворотні лапки (`` ` ``) та синтаксис `${вираз}`, щоб спростити цей процес. Крім основного `${змінна}`, можна використовувати будь-який вираз, наприклад, `${1 + 1}` або `${fullName.toUpperCase()}`. Ця інтерполяція обчислюється у рантаймі, тобто ваші вирази розраховуються, коли код виконується.)

Alternatives include string concatenation, template libraries, or even manual creation of string expressions. Yet, for most TypeScript or JavaScript jobs, string interpolation remains the go-to for clean and efficient code.

(Альтернативами можуть бути конкатенація рядків, бібліотеки шаблонів або навіть ручне створення строкових виразів. Однак для більшості завдань у TypeScript або JavaScript інтерполяція рядків залишається найкращим рішенням для чистого та ефективного коду.)

## See Also
(Дивись Також)

- MDN Web Docs on Template Literals: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- TypeScript Official Documentation: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- String interpolation in modern JS frameworks (React, Angular): https://blog.logrocket.com/a-guide-to-component-rendering-and-string-interpolation-in-react-and-angular/

MDN Web Docs or TypeScript documentation provide solid groundwork, while the blog post on LogRocket dives into how frameworks like React and Angular handle string interpolation within their ecosystems.

(MDN Web Docs та офіційна документація TypeScript надають міцну основу, тоді як стаття на LogRocket детально розглядає, як фреймворки на кшталт React та Angular обробляють інтерполяцію рядків у своїх екосистемах.)
