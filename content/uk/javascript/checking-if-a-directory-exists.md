---
title:    "Javascript: Перевірка наявності каталогу"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Тому
Перевірка існування директорії - важливий аспект програмування, оскільки дозволяє перевірити, чи необхідні дані або файли є доступними для використання у вашій програмі.

## Як це зробити
Існує кілька способів перевірити існування директорії в Javascript. Найпростішим способом є використання методу `existsSync` з модуля `fs` (файлової системи). Давайте подивимося на приклад коду нижче:

```Javascript
// Підключення модуля fs
const fs = require('fs');

// Перевірка існування директорії
if (fs.existsSync('/шлях/до/директорії')) {
    console.log('Директорія існує!');
} else {
    console.log('Директорія не існує!');
}
```

У цьому прикладі ми використовуємо метод `existsSync` для перевірки існування директорії під шляхом `/шлях/до/директорії`. Якщо директорія існує, то виконається перша частина `if` оператору, яка виведе повідомлення "Директорія існує!". Якщо директорія не існує, то виконається друга частина `if` оператору і виведеться повідомлення "Директорія не існує!".

## Глибшій розбір
Для тих, хто бажає дізнатися більше про перевірку існування директорії, варто згадати про інші методи, які також можуть бути використані. Наприклад, метод `access` з модуля `fs` дозволяє перевірити наявність директорії, але вже з використанням колбек-функції. Також варто зазначити, що перевірка існування директорії не може бути виконана без певних прав доступу до файлової системи.

# Дивись також
- [Метод `existsSync` у документації Node.js](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Приклад використання методу `access` для перевірки існування директорії](https://www.geeksforgeeks.org/node-js-fs-access-method/)
- [Робота з файловою системою в Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)