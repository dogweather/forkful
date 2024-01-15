---
title:                "Створення тимчасового файлу."
html_title:           "Javascript: Створення тимчасового файлу."
simple_title:         "Створення тимчасового файлу."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

НЕОБХІДНІСТЬ: Створення тимчасового файлу є важливою операцією для збереження тимчасових даних під час виконання програми. Це дозволяє економити пам'ять і уникнути конфліктів з іншими файлами.

Як це зробити:
```Javascript
// Створення тимчасового файлу з унікальним іменем
const fs = require('fs');
const { tmpdir } = require('os');
const { randomBytes } = require('crypto');

const fileName = randomBytes(16).toString('hex');
const filePath = path.join(tmpdir(), fileName)

fs.writeFileSync(filePath, 'Hello World');
console.log('Тимчасовий файл створено:', filePath);
// Вихід: Тимчасовий файл створено: /tmp/31aba4e03e4e4103b12121d9f41d629c
```

Глибше:
Створення тимчасового файлу можна виконати за допомогою модулів `fs`, `os` та `crypto`. Спочатку потрібно імпортувати ці модулі. Для генерації унікального імені використовуємо функцію `randomBytes` з модуля `crypto`, яка повертає випадкову послідовність байтів. Потім, за допомогою функції `writeFileSync` з модуля `fs`, створюємо файл і передаємо йому шлях до нашого тимчасового каталогу `tmpdir()`, разом з унікальним ім'ям файлу. У результаті, тимчасовий файл буде створений та збережений за заданим шляхом.

Докладніше про створення тимчасового файлу можна прочитати в офіційній документації Node.js: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_createtempfile_prefix_options_callback

Дивіться також:
- Офіційна документація Node.js: https://nodejs.org/dist/latest-v14.x/docs/api/
- Приклади використання модулів: https://www.w3schools.com/nodejs/nodejs_filesystem.asp