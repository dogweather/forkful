---
title:                "Створення тимчасового файлу"
aliases: - /uk/javascript/creating-a-temporary-file.md
date:                  2024-01-20T17:41:01.237641-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Створення тимчасових файлів дає можливість зберігати дані, які потрібні тільки під час одного сеансу роботи програми. Програмісти користуються цим, щоб мати проміжне сховище для даних, згодом ці дані можна видалити, так не захаращуючи систему.

## Як це робити:

Для створення тимчасових файлів у JavaScript можна використовувати модуль `fs` у Node.js. Ось базовий приклад:

```javascript
const fs = require('fs');
const os = require('os');
const path = require('path');

// Создание уникального тимчасового файла
const tmpFile = path.join(os.tmpdir(), 'my-app.tmp');

// Запис даних в файл
fs.writeFile(tmpFile, 'Тестируем тимчасовий файл!', (err) => {
  if (err) throw err;

  console.log('Тимчасовий файл створений і записаний.');

  // Читання та вивід вмісту тимчасового файла
  fs.readFile(tmpFile, 'utf8', (err, data) => {
    if (err) throw err;
    console.log('Вміст тимчасового файла:', data);

    // Видалення тимчасового файла
    fs.unlink(tmpFile, (err) => {
      if (err) throw err;
      console.log('Тимчасовий файл видалений.');
    });
  });
});
```

## Поглиблений огляд

Створення тимчасових файлів є старою практикою. У UNIX-подібних ОС існує `/tmp` директорія для таких цілей. В Windows, тимчасові файли зазвичай зберігаються в `%TEMP%`. У Node.js для роботи з файлами використовується вбудований модуль `fs`, що дозволяє працювати із файловою системою. 

Окрім `fs`, існують бібліотеки, які можуть допомогти з тимчасовими файлами, наприклад, `tmp`. Вона дозволяє більш гнучко управляти тимчасовими файлами та директоріями, подбає про їх видалення або надасть вам більше опцій для контролю люку циклу життя.

## Дивіться також:

- Node.js File System Documentation: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- `os` module documentation, for OS-related utilities: [https://nodejs.org/api/os.html](https://nodejs.org/api/os.html)
- `tmp` npm module for even easier management of temporary files: [https://www.npmjs.com/package/tmp](https://www.npmjs.com/package/tmp)
