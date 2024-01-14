---
title:    "Javascript: Перевірка наявності каталогу."
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Бажаєте перевірити, чи існує директорія у вашому Javascript коді? Така перевірка може бути корисною, якщо ви хочете перевірити наявність певної папки перед виконанням додаткових дій з файлами.

## Як

Можна перевірити наявність директорії за допомогою методу `existsSync()` з глобального об'єкта `fs`. Оберіть шлях до папки і передайте його як аргумент методу. Приклад коду:

```Javascript
const fs = require('fs');

let directory = './assets'; // шлях до директорії
let exists = fs.existsSync(directory); // перевірка існування директорії

console.log(`Директорія ${directory} ${exists ? 'існує' : 'не існує'}`); // виведення результату
```

В даному прикладі ми перевіряємо існування директорії зі шляхом `./assets` та отримуємо вивід `Директорія ./assets існує`.

## Deep Dive

При перевірці існування директорії, метод `existsSync()` повертає `true`, якщо шлях вказує на існуючу директорію, або `false`, якщо шлях вказує на неіснуючу директорію. Цей метод також перевіряє існування симлінків.

При перевірці існування папки, бажано також перевірити, чи це дійсно директорія за допомогою методу `isDirectory()` з модуля `fs`:

```Javascript
const fs = require('fs');

let directory = './assets';
let exists = fs.existsSync(directory);

if (exists && fs.lstatSync(directory).isDirectory()) { // перевірка існування та типу
    // додаткові дії, якщо це директорія
} else {
    // додаткові дії, якщо це не директорія або її не існує
}
```

Також бажано обробляти виключення, якщо шлях до директорії вказує на файл, а не на директорію, за допомогою `isFile()` методу.

## Дивіться також

- [Метод `existsSync()`](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Функція `lstatSync()`](https://nodejs.org/api/fs.html#fs_lstatsync_path_options)
- [Метод `isDirectory()`](https://nodejs.org/api/fs.html#fs_fs_isdirectory_path_callback)
- [Метод `isFile()`](https://nodejs.org/api/fs.html#fs_fs_isfile_path_callback)