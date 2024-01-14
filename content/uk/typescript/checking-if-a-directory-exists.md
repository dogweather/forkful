---
title:                "TypeScript: Перевірка існування каталогу"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Чому

Найбільш часто під час програмування виникає необхідність перевірити наявність певної директорії на комп'ютері. Це може бути необхідно для створення нового файлу, чи маніпуляцій з вже існуючими. Необхідність перевірки наявності директорії може виникнути в різних ситуаціях, тому важливо знати як це правильно зробити за допомогою TypeScript.

##Як це зробити

Перевіряти наявність директорії можна за допомогою вбудованої функції `fs.existsSync()` в TypeScript. Ця функція приймає шлях до директорії яку необхідно перевірити і повертає `true`, якщо директорія існує або `false`, якщо вона відсутня. Давайте переглянемо приклад реалізації цієї функції:

```TypeScript
import fs from "fs";

const directoryPath = "./my-directory";

if (fs.existsSync(directoryPath)) {
  console.log("Директорія існує!");
} else {
  console.log("Директорія не існує!");
}
```

У цьому прикладі ми імпортуємо модуль `fs` і використовуємо функцію `existsSync()` для перевірки наявності директорії `my-directory`. Залежно від результату, виводиться відповідне повідомлення.

##Deep Dive

Для тих, хто хоче дізнатися більше про перевірку наявності директорії в TypeScript, варто звернути увагу на методи, які можна використовувати разом з `existsSync()`. Наприклад, `fs.isDirectory()` може бути використано для перевірки чи є даний шлях директорією. Також можна використовувати метод `fs.access()` для перевірки чи є даний шлях доступним для читання або запису.

##Дивись також

- [Документація Node.js про `fs.existsSync()`](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Блог-пост "Як перевірити наявність директорії в TypeScript"](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-a-remote-location-with-typescript)