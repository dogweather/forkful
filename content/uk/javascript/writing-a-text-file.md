---
title:                "«Написання текстового файлу»"
html_title:           "Javascript: «Написання текстового файлу»"
simple_title:         "«Написання текстового файлу»"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу може бути корисним в багатьох ситуаціях, наприклад, для збереження важливої інформації або для створення звітів із зібраних даних. Це також може бути корисно для розробки програм, які потребують вводу тексту користувача.

## Як це зробити

Щоб написати текстовий файл у Javascript, використовуйте функцію `fs.writeFile()` з модуля `fs`. Нижче наведений приклад коду, який створює новий файл з назвою "example.txt" і заповнює його вмістом "Hello, world!".

```Javascript 
const fs = require('fs');
fs.writeFile('example.txt', 'Hello, world!', function (err) {
  if (err) throw err;
  console.log('File created and saved!');
});
```

У цьому прикладі ми використовуємо функцію `writeFile()` з трьома параметрами: назва файлу, який потрібно створити, вміст файлу і функцію зворотного виклику, яка повідомить нас про те, чи вдалося створити файл.

## Детальніше

Функція `writeFile()` є частиною вбудованого модуля `fs` у Javascript, який дозволяє здійснювати операції з файлами на комп'ютері. Окрім створення файлів, використовуючи `writeFile()`, цей модуль також дозволяє зчитувати, перейменовувати, видаляти та інші операції з файлами.

Для детальнішої інформації про написання текстових файлів на Javascript, ви можете переглянути офіційну документацію `fs` модуля [тут](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback).

## Дивіться також

- [Документація по модулю fs](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Стаття про запис файлів у Javascript](https://flaviocopes.com/nodejs-write-file/)
- [Відео урок про роботу з файлами на Javascript](https://www.youtube.com/watch?v=wCKnWb9l7iU)