---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що та чому?

Інтерполяція стрічок - це процес вставки значень змінних або виразів прямо в текстову стрічку. Програмісти це роблять, щоб створювати динамічний контент або форматувати повідомлення, замість нудних конкатенацій.

## Як таке зробити:

```TypeScript
let name = "Вася";
console.log(`Привіт, ${name}!`);
```
Виводить: "Привіт, Вася!"

```TypeScript
let apples = 5;
console.log(`У мене є ${apples} яблук.`);
```
Виводить: "У мене є 5 яблук."

```TypeScript
let x = 10;
let y = 20;
console.log(`Сума чисел ${x} та ${y} є ${x + y}.`);
```
Виводить: "Сума чисел 10 та 20 дорівнює 30."

## Поглиблений огляд

Інтерполяцію стрічок можна знайти по всьому програмниому світі, від Perl і PHP в 90-х, до нинішнього JavaScript і TypeScript. Це альтернатива старій методиці конкатенації або використанню форматів подібних до printf(), яка є суттєво більш читабельним.
Головна ціль інтерполяції стрічок в TypeScript полягає в упрощенні синтаксису і зробити його більш зрозумілим.

## Дивіться також:

1. [Офіційна документація по TypeScript](https://www.typescriptlang.org/docs/)
2. [MDN: Інтерполяція собственної стрічки](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Template_literals)
3. [StackOverflow: Інтерполяція стрічки в TypeScript](https://stackoverflow.com/questions/3304014)