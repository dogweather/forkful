---
title:                "Створення текстового файлу"
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Запис текстового файлу - це процес збереження даних у виді тексту на диску. Програмісти роблять це для зберігання конфігурацій, логування дій програми чи збереження користувацьких даних.

## Як це зробити:

```TypeScript
import { writeFile } from 'fs';

const data = 'Привіт, це текстовий файл!';
writeFile('example.txt', data, (err) => {
    if (err) throw err;
    console.log('Файл успішно записано!');
});
```

При виконанні цього скрипта, у папці проекту з'явиться файл `example.txt` з вмістом "Привіт, це текстовий файл!", а в консолі відобразиться повідомлення "Файл успішно записано!".

## Поглиблений огляд

У минулому для запису файлів використовувалися більш низькорівневі операційні системи API. Сьогодні в Node.js fs модуль дозволяє взаємодіяти з файлами на високому рівні. Як альтернативи, можна використовувати `fs.promises` для роботи з промісами або async/await підхід. Важливо розуміти, що запис файлу може бути синхронним або асинхронним, що впливає на продуктивність додатка.

## Додаткові матеріали

- Node.js fs.writeFile документація: https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback
- Посібник з Node.js FileSystem API: https://nodejs.dev/learn/the-nodejs-fs-module
- Туторіал з використанням async/await для роботи з файлами: https://www.sitepoint.com/modern-asynchronous-javascript-with-async-await/
