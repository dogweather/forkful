---
title:                "Javascript: Перевірка наявності директорії"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування директорії є важливим етапом в програмуванні, оскільки дозволяє переконатися, що система правильно виконує дії з файлами та директоріями.

## Як

```Javascript
// Підключення модуля fs для роботи з файловою системою
const fs = require('fs');

// Перевірка існування директорії "images"
// Виводиться строки "Директорія images існує" або "Директорія images не існує" в залежності від результату
if (fs.existsSync("./images")) {
  console.log("Директорія images існує");
} else {
  console.log("Директорія images не існує");
}
```

Результат для директорії, яка існує:

```
Директорія images існує
```

Результат для директорії, яка не існує:

```
Директорія images не існує
```

## Deep Dive

Перевірка існування директорії виконується за допомогою методу `existsSync()` з модуля `fs`. Цей метод приймає шлях до директорії в якості аргументу та повертає `true` або `false` в залежності від того, чи існує директорія за вказаним шляхом. Цей шлях може бути відносним або абсолютним.

## See Also

- [Модуль fs в документації Node.js](https://nodejs.org/api/fs.html)
- [Порівняння абсолютного та відносного шляху до файлу](https://www.digitalocean.com/community/tutorials/absolute-vs-relative-paths-unix)
- [Використання модуля path для роботи з шляхами в Node.js](https://nodejs.org/api/path.html)