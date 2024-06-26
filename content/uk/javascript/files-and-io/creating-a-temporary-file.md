---
date: 2024-01-20 17:41:01.237641-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0414\
  \u043B\u044F \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\
  \u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\u0430\u0439\u043B\u0456\u0432\
  \ \u0443 JavaScript \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043C\u043E\u0434\u0443\
  \u043B\u044C `fs` \u0443 Node.js. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\u0432\
  \u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:50.026728-06:00'
model: gpt-4-1106-preview
summary: "\u0414\u043B\u044F \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0442\u0438\u043C\u0447\u0430\u0441\u043E\u0432\u0438\u0445 \u0444\u0430\u0439\
  \u043B\u0456\u0432 \u0443 JavaScript \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043C\
  \u043E\u0434\u0443\u043B\u044C `fs` \u0443 Node.js."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0438\u043C\u0447\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 21
---

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
