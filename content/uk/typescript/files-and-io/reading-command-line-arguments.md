---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:57:50.314976-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Читання аргументів командного рядка дозволяє вашій програмі отримувати вхідні дані при її запуску. Програмісти роблять це, щоб зробити свої програми гнучкішими та керованими без зміни коду.

## Як це зробити:

Щоб прочитати аргументи командного рядка в TypeScript, вам знадобиться Node.js та пара рядків коду. Ось приклад:

```TypeScript
// У файлі index.ts

// Отримуємо аргументи командного рядка, починаючи з третього елементу масиву,
// оскільки перші два - це шлях до runtime та виконуваного файлу.
const args = process.argv.slice(2);

console.log('Отримані аргументи командного рядка:', args);

// Запуск програми: 
// node index.ts hello world
// Виведення:
// Отримані аргументи командного рядка: [ 'hello', 'world' ]
```

## Поглиблений огляд

Читання аргументів командного рядка - стара добра техніка з часів, коли інтерфейси командного рядка були найпоширенішим способом взаємодії з комп'ютерами. Щодо альтернатив, можна використовувати бібліотеки як `commander` або `yargs` для парсингу керованих аргументів та флагів. Вони забезпечують потужні функції, такі як валідація, задання аргументів за умовчанням, та автоматичну генерацію довідки. У TypeScript можна прочитати аргументи за допомогою `process.argv`, яке вбудоване в Node.js, для прямого отримання вхідних даних без додаткових залежностей.

## Додаткові ресурси

- Node.js `process.argv` документація: [https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `commander` NPM пакет: [https://www.npmjs.com/package/commander](https://www.npmjs.com/package/commander)
- `yargs` NPM пакет: [https://www.npmjs.com/package/yargs](https://www.npmjs.com/package/yargs)
- Гайд по використанню командного рядка: [https://www.codecademy.com/articles/command-line-commands](https://www.codecademy.com/articles/command-line-commands)
