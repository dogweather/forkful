---
title:                "Створення текстового файлу"
date:                  2024-01-19
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і Чому?
Запис текстового файлу — це процес збереження даних у форматі, що читається людьми. Програмісти роблять це для логування, збереження налаштувань, експорту даних.

## Як це робити:
У JavaScript, для запису файлів на стороні сервера, використовують Node.js. На прикладі fs модуля:

```javascript
const fs = require('fs');
const data = 'Привіт, це текстовий файл.';

fs.writeFile('textFile.txt', data, 'utf8', (err) => {
    if (err) throw err;
    console.log('Файл було створено!');
});
```

Коли запустиш код, з'являється файл `textFile.txt` з нашим повідомленням.

## Поглиблено:
Запис файлів у Node.js з'явився з появою платформи в 2009 році. Альтернативами є використання баз даних, об'єктів Blob у браузері або cloud services. Деталі: модуль `fs` використовує системні виклики, асинхронність Node.js уможливлює ефективну роботу з I/O операціями. 

## Дивись також:
- Node.js `fs` documentation: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN Веб-API для роботи з файлами: [https://developer.mozilla.org/en-US/docs/Web/API/File](https://developer.mozilla.org/en-US/docs/Web/API/File)
- Stream-handling in Node.js: [https://nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)
