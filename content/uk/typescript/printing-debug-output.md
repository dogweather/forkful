---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це й навіщо?

Друк відлагоджувального виведення - це процес виведення значень змінних, поточних станів або повідомлень для налагодження програми. Програмісти використовують це, щоб знайти і виправити помилки у коді.

## Як це запустити:

Для друку відлагоджувального виведення в TypeScript ви можете використовувати команду `console.log()`. Ось приклад:

```TypeScript 
let a: number = 5;
console.log('Значення a:', a);
```

Цей код виведе `Значення a: 5` у консоль.

## Поглиблене дослідження:

Друк відлагоджувального виведення - це один з найзручніших інструментів налагодження, який був позичений TypeScript з JavaScript. Крім `console.log()`, існують й інші способи друку відлагоджувального виведення, такі як `console.error()`, `console.warn()`, що дозволяють категоризувати повідомлення. 

Однією з особливостей `console.log()` в TypeScript є те, що вона може бути скомпільована або відключена за допомогою параметрів компілятора. Специфічно, ви можете використовувати параметр `--removeComments` для видалення `console.log()` при компіляції.

## Дивіться також:

1. [TypeScript - Робота з консоллю](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-1.html#console)
2. [TypeScript - Параметри компілятора](https://www.typescriptlang.org/tsconfig)