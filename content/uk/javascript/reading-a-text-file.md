---
title:                "Javascript: Читання текстового файлу"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Програмування на Javascript - це захоплююча тема, а читання текстових файлів є важливою складовою цього процесу. Цей блог допоможе вам розібратися з основами читання текстового файлу в Javascript.

## Як

```Javascript
// Приклад коду для читання текстового файлу
const fs = require('fs');

// Використовуємо метод readFile для зчитування файлу
fs.readFile('textfile.txt', 'utf8', (err, data) => {
    // Перевіряємо наявність помилок
    if (err) throw err;
    // Виводимо зчитаний текст в консоль
    console.log(data);
});

```

В результаті ви отримаєте вміст текстового файлу, що записаний у змінну `data`.

```
Hello, world!
This is a text file.
```

## Глибоке погруження

Читання текстового файлу у Javascript може бути більш детальним процесом, в залежності від вашої потреби. Ви можете використовувати `fs.readFile()` або `fs.readFileSync()` для зчитування файлу, налаштовуючи параметри для отримання більш точного результату.

Наприклад, ви можете використовувати параметр `encoding` для вказання кодування файлу, яке відповідає його змісту. Крім того, ви можете налаштувати декодування `Buffer`, використовуючи параметр `flag`.

```
// Використовування параметрів для зчитування файлу
fs.readFile('textfile.txt', {encoding: 'latin1', flag: 'r'}, (err, data) => {
    // Перевіряємо наявність помилок
    if (err) throw err;
    // Виводимо зчитаний текст в консоль
    console.log(data);
});
```

## Дивись також

- [fs.readFile() документація](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [fs.readFileSync() документація](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)
- [Навчальне відео по читанню файлів в Javascript](https://www.youtube.com/watch?v=CBQGl6zokMs)