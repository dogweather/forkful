---
title:    "Javascript: Читання текстового файлу"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому 

Програмування на Javascript є важливою навичкою для кожного начинаючого розробника. Через те, що багато веб-сайтів та програм використовують текстові файли для зберігання даних, знання, як читати ці файли, є необхідним для ефективної роботи з ними.

## Як це зробити 

Для того, щоб прочитати текстовий файл в Javascript, ми можемо використовувати вбудовану функцію "fs.readFile". Приклад коду для читання файлу з назвою "data.txt" з вмістом "Hello world" виглядатиме наступним чином:

```Javascript
const fs = require('fs');

fs.readFile('data.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data); // виводить "Hello world"
});
```

## Заглиблення 

Крім вбудованої функції "fs.readFile", є ще багато інших методів для читання текстових файлів в Javascript. Наприклад, ми можемо використовувати функцію "fs.readFileSync", яка повертає дані з файлу без асинхронного підходу. Також, ми можемо використовувати пакет "fs-extra" для читання файлів зі збереженням усіх даних у зручному для нас форматі.

## Дивись також 

- [Офіційна документація по функції "fs.readFile"](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Навчальне відео по читанню файлів на Javascript](https://www.youtube.com/watch?v=QSpwocGaiJ8)
- [Огляд різних методів читання файлів в Javascript](https://blog.bitsrc.io/6-ways-to-write-better-code-with-node-js-e0b54d5f065a)