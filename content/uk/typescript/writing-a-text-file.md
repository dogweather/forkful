---
title:                "TypeScript: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текствого файлу є важливим навичкою для будь-якого програміста, оскільки це дозволяє зберігати та передавати інформацію в зрозумілому для комп'ютера форматі.

## Як

```TypeScript
// Створення нового текстового файлу з використанням вбудованого модулю fs
const fs = require('fs');
fs.writeFile('hello.txt', 'Привіт, світе!', function (err) {
  if (err) throw err;
  console.log('Файл успішно створений!');
});
```

## Deep Dive

Написання текстового файлу може бути простим завданням, але варто врахувати деякі особливості. Наприклад, необхідно враховувати кодування для правильного запису кирилиці або спеціальних символів. Також важливо не забувати закривати файл після закінчення роботи з ним.

## Дивіться також
- [Документація з побудови файлової системи в TypeScript](https://www.typescriptlang.org/docs/handbook/filesystems.html)
- [Онлайн курс "Написання текстового файлу в TypeScript" на Pluralsight](http://www.pluralsight.com/courses/typescript-text-file-writing)
- [Стаття про роботу з текстовими файлами на Medium](https://medium.com/dailyjs/how-to-read-and-write-text-files-in-node-js-fa4da6562a05)