---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Створення тимчасового файлу в JavaScript означає локальне зберігання даних безпосередньо на віддаленому сервері. Програмісти створюють тимчасові файли, коли їм тимчасово потрібно зберігати дані, які вони не хочуть втратити, наприклад, при переході між сторінками.

## Як це Робити:

Ось кілька прикладів вашого коду:

```Javascript
const os = require('os');
const fs = require('fs');
const path = require('path');

function createTempFile(filename, content) {
    const tempDir = os.tmpdir();
    const filePath = path.join(tempDir, filename);

    fs.writeFile(filePath, content, (err) => {
        if (err) throw err;
        console.log('Temp file write succeeded.');
    });
}

createTempFile('temp.txt', 'This is some sample text.')
```

Коли ви запустите цей код, він створить тимчасовий файл із іменем 'temp.txt' і змістом 'This is some sample text.' в вашій тимчасовій директорії.

## Поглиблений Огляд:

Створення тимчасових файлів - це часто використовувана робота з файлами у JavaScript з часів Node.js. Альтернативою може бути використання бази даних, але це може бути надмірним для простих завдань.

У той час як створення тимчасового файлу може здаватися простим, воно включає в себе важливі деталі, такі як коректне оброблення помилок і правильне видалення після використання.

##Дивіться також:

Для більш глибокого розуміння, вам можуть бути корисними наступні ресурси:

- Node.js File System API: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- JavaScript Promises: [https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Using_promises](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Using_promises)
- Path module in Node.js: [https://nodejs.org/api/path.html](https://nodejs.org/api/path.html)