---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Читання текстових файлів, це процес отримання інформації з файлу, збереженої в текстовому форматі. Програмісти роблять це для обробки даних, збережених в таких файлах, як налаштування, логи, документація тощо.

## Як це робити:

Щоб прочитати текстовий файл в JavaScript, використовуємо вбудований модуль `fs` (File System). Подивимося на приклад:

```Javascript
const fs = require('fs');

fs.readFile('MyFile.txt', 'utf8', function(err, data){
    if (err) throw err;
    console.log(data);
});
```

Код вище читає вміст файлу `MyFile.txt` і виводить його в консоль. Якщо під час читання файлу виникає помилка, код виведе цю помилку.

## Поглиблений аналіз

Історично, читання файлів в JavaScript було обмежене оскільки це клієнтська мова. Але з приходом Node.js, JavaScript отримав доступ до файлової системи, що дозволяє читати й записувати файли.

Альтернативою для `fs` є модуль `fs.promises`, який використовує проміси замість зворотніх викликів, що робить код чистішим і зрозумілішим.

Зверніть увагу, що читання файлів - це блокуюча операція, що може вплинути на продуктивність. Тому для великих файлів рекомендується використовувати потоки.

## Дивіться також

- Документація Node.js про модуль 'fs': [Посилання](https://nodejs.org/api/fs.html)
- MDN Web Docs - Читання файлів у браузері: [Посилання](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
- Блог про читання і запис файлів в Node.js: [Посилання](https://blog.logrocket.com/reading-writing-files-in-node-js/)