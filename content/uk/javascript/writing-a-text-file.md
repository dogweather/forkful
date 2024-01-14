---
title:                "Javascript: Написання текстового файлу"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Чому: Записування текстового файлу - важливий елемент програмування, оскільки це дозволяє зберігати дані та надавати їм структуру для подальшої обробки.

## Як: Для створення текстового файлу використовуються наступні кроки:
```Javascript
// Підключення вбудованого модуля "fs"
const fs = require('fs');

// Створення нового текстового файлу з назвою "myFile.txt" та вмістом "Привіт, світ!"
fs.writeFile('myFile.txt', 'Привіт, світ!', (err) => {
    if (err) throw err;
    console.log('Файл створено успішно.');
});
```

В результаті виконання коду, у вас буде створений файл з назвою "myFile.txt", який містить текст "Привіт, світ!".

## Deep Dive: Під час записування текстового файлу, необхідно враховувати деякі особливості:

- Ім'я файлу повинно бути унікальним для уникнення конфліктів.
- Для збереження файлу в іншій директорії, необхідно вказати шлях до неї у першому аргументі методу `writeFile`.
- Якщо вам потрібно додати новий вміст до вже існуючого файлу, ви можете використовувати метод `appendFile` замість `writeFile`.

# Дивись також:
- [Документація Node.js про запис файлів](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Поради з правильного використання файлової системи в Node.js](https://www.digitalocean.com/community/tutorials/how-to-work-with-the-node-js-file-system-module-uk)