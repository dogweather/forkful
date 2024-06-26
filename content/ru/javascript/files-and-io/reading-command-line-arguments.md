---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:33.958327-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043F\u0440\u044F\u043C\u043E\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u044D\u0442\
  \u043E \u0432 Node.js."
lastmod: '2024-03-13T22:44:45.788321-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u044F\u043C\u043E\u0439 \u0441\u043F\u043E\
  \u0441\u043E\u0431 \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u044D\u0442\u043E\
  \ \u0432 Node.js."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 23
---

## Как это сделать:
Вот прямой способ сделать это в Node.js:

```javascript
// process.argv содержит аргументы командной строки
const args = process.argv.slice(2);

console.log(args);

// Запустите этот скрипт с: node yourscript.js firstArg secondArg
```

Пример выходных данных, если вы запустите `node yourscript.js pineapple 42`:

```javascript
['pineapple', '42']
```

Использование пакета, такого как `yargs`, упрощает жизнь, позволяя определять и получать доступ к аргументам по имени.

```javascript
// Установить с помощью npm install yargs
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// Запустите это с: node yourscript.js --fruit pineapple --number 42
```

И вы получите:

```javascript
{ fruit: 'pineapple', number: '42' }
```

Чисто и ясно, с именованными параметрами.

## Углубляемся
В старину аргументы считывались в C с помощью `argc` и `argv` в функции `main`. В Node.js используется `process.argv`. Это массив, где первый элемент — это путь к исполняемому файлу node, второй — имя файла скрипта, а остальные — ваши собственные аргументы.

`yargs` полезен для сложных приложений: он анализирует аргументы в удобный объект, управляет значениями по умолчанию и даже автоматически генерирует сообщения справки.

Есть также пакет `minimist`, более легкая альтернатива `yargs`, если вам нравится минимализм.

В глубине души Node.js использует `process.binding('options')` от V8 для разбора, который не доступен обычному пользователю. Этот внутренний метод обладает огромным количеством возможностей под капотом, управляя разбором и получением параметров командной строки.

## Смотрите также
- Документация Node.js process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- GitHub репозиторий Yargs: https://github.com/yargs/yargs
- Minimist на npm: https://www.npmjs.com/package/minimist
