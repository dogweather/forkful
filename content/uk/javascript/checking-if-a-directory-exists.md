---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:57:35.116137-07:00
simple_title:         "Перевірка наявності директорії"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Перевірка існування директорії дозволяє уникнути помилок перед читанням чи записом файлів. Програмісти виконують таку перевірку щоб не спричинити збої у програмі при доступі до неіснуючих папок.

## How to: (Як це зробити:)
Використовуйте модуль `fs` з Node.js, щоб перевірити наявність директорії:

```Javascript
const fs = require('fs');
const path = './path/to/directory';

try {
  if (fs.statSync(path).isDirectory()) {
    console.log('Directory exists.');
  }
} catch (e) {
  if (e.code === 'ENOENT') {
    console.log('Directory does not exist.');
  } else {
    console.error('An error occurred:', e);
  }
}
```

Цей код дасть наступний вивід:
- Якщо директорія існує: `Directory exists.`
- Якщо директорії немає: `Directory does not exist.`
- Якщо виникла інша помилка: `An error occurred: [Error object]`

## Deep Dive (Занурення глибше):
Перевірка існування директорії важлива з часів Unix. На початку, програмісти вручну перевіряли файлову систему, але це неефективно. Node.js використовує асинхронне виконання для ефективності, але ми показали синхронний метод для простоти.

Альтернативою `fs.statSync()` є використання `fs.existsSync()`, який повертає `true` чи `false` без виключень:

```Javascript
const directoryExists = fs.existsSync(path);
console.log(directoryExists ? 'Directory exists.' : 'Directory does not exist.');
```

Проте використання синхронних методів може сповільнити ваш додаток, якщо ви виконуєте багато операцій з файловою системою. Розгляньте асинхронні варіанти для продакшен сценаріїв.

## See Also (Дивіться також):
- Node.js fs Docs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- Блог про асинхронне програмування в Node.js: [https://blog.risingstack.com/mastering-async-await-in-nodejs/](https://blog.risingstack.com/mastering-async-await-in-nodejs/)
