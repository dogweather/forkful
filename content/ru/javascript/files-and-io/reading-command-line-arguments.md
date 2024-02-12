---
title:                "Чтение аргументов командной строки"
aliases:
- /ru/javascript/reading-command-line-arguments/
date:                  2024-01-29T00:01:33.958327-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Чтение аргументов командной строки означает захват дополнений, которые пользователи добавляют к командам, когда запускают ваш скрипт. Программисты делают это, чтобы пользователи могли настраивать поведение без изменения кода.

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
