---
title:                "Чтение аргументов командной строки"
aliases:
- /ru/typescript/reading-command-line-arguments/
date:                  2024-01-29T00:01:03.667066-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Аргументы командной строки позволяют пользователям передавать данные программе при ее запуске. Программисты используют их для настройки поведения программы без изменения кода.

## Как это делается:

В TypeScript для чтения аргументов командной строки используется Node.js. Вот как это делается:

```typescript
// Необходимо импортировать process из Node.js
import process from 'process';

// Получаем аргументы командной строки, начиная с третьей позиции
const args = process.argv.slice(2);

console.log('Аргументы командной строки:', args);
```

Запустите этот скрипт как `ts-node yourscript.ts arg1 arg2` и увидите:

```
Аргументы командной строки: ['arg1', 'arg2']
```

## Погружение

В древние времена командной строки вся взаимодействие с пользователем происходило через текст. Linux, UNIX и Windows использовали аргументы командной строки, чтобы сообщать программам, что делать.

Теперь о альтернативах: кроме `process.argv`, в Node.js можно использовать библиотеки типа `yargs` или `commander` для дополнительных функций, таких как разбор и проверка.

Суть этого в TypeScript проста: `process.argv` - это массив со всеми аргументами. Индекс 0 - путь к Node, индекс 1 - путь к скрипту, так что настоящие аргументы начинаются с индекса 2.

## Смотрите также

Для дальнейшего изучения начните с этого:

- [Документация Node.js process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Репозиторий Yargs на GitHub](https://github.com/yargs/yargs)
- [Репозиторий Commander.js на GitHub](https://github.com/tj/commander.js)
