---
date: 2024-01-20 17:57:50.314976-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0429\u043E\u0431 \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u0438 \u0430\
  \u0440\u0433\u0443\u043C\u0435\u043D\u0442\u0438 \u043A\u043E\u043C\u0430\u043D\u0434\
  \u043D\u043E\u0433\u043E \u0440\u044F\u0434\u043A\u0430 \u0432 TypeScript, \u0432\
  \u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u044C\u0441\u044F\
  \ Node.js \u0442\u0430 \u043F\u0430\u0440\u0430 \u0440\u044F\u0434\u043A\u0456\u0432\
  \ \u043A\u043E\u0434\u0443. \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\u0430\
  \u0434."
lastmod: '2024-03-13T22:44:48.892205-06:00'
model: gpt-4-1106-preview
summary: "\u0429\u043E\u0431 \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u0438\
  \ \u0430\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u0438 \u043A\u043E\u043C\u0430\
  \u043D\u0434\u043D\u043E\u0433\u043E \u0440\u044F\u0434\u043A\u0430 \u0432 TypeScript,\
  \ \u0432\u0430\u043C \u0437\u043D\u0430\u0434\u043E\u0431\u0438\u0442\u044C\u0441\
  \u044F Node.js \u0442\u0430 \u043F\u0430\u0440\u0430 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u043A\u043E\u0434\u0443."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
weight: 23
---

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
