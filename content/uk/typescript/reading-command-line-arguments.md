---
title:    "TypeScript: Читання аргументів командного рядка"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Чому

Читання командних аргументів є важливим аспектом програмування TypeScript. Воно дозволяє програмі отримувати вхідні дані від користувача при запуску з командного рядка.

## Як

Щоб прочитати командні аргументи, використовуйте ```process.argv``` глобальний об'єкт у TypeScript. Ось приклад коду:

```TypeScript
const args = process.argv.slice(2);
console.log(args);
```

При виконанні цього коду у командному рядку з введеними аргументами ```node index.ts arg1 arg2``` ви отримаєте наступний результат:

```TypeScript
['arg1', 'arg2']
```

В цьому прикладі ми використали метод ```slice()```, щоб отримати аргументи, які не є шляхом до виконуваного файлу та іменем файлу. Ви можете використовувати інші методи для отримання конкретних аргументів, які потрібні для вашої програми.

## Огляд

Читання командних аргументів дозволяє зробити вашу програму більш гнучкою, дозволяючи користувачам передавати параметри при запуску. Ви можете також перевірити вказані параметри та зробити потрібні дії в залежності від них.

## Дивись також

- [Deno Docs: Command Line Arguments](https://deno.land/manual/examples/command_line_args)
- [Node.js Docs: Process Object](https://nodejs.org/api/process.html)
- [Stack Overflow: How to get command line arguments in TypeScript](https://stackoverflow.com/questions/43515246/how-to-get-command-line-arguments-in-typescript)