---
title:                "Перевірка наявності каталогу"
html_title:           "TypeScript: Перевірка наявності каталогу"
simple_title:         "Перевірка наявності каталогу"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

Перевірка існування папки є необхідним елементом багатьох програм. Вона дозволяє перевіряти наявність файлів і папок перед їх обробкою, що дозволяє уникати помилок і зберігати чистий і організований код.

## Як це зробити

```TypeScript
// Імпортуємо метод existsSync з модуля fs
import { existsSync } from 'fs';

// Перевіряємо існування папки за допомогою методу existsSync
if (existsSync('./папка')) {
  console.log('Папка існує!');
} else {
  console.log('Папка не існує!');
};
```

Приклад виводу консолі: `Папка існує!`

Щоб перевірити, чи існує папка, ми імпортуємо метод `existsSync` з модуля `fs` та передаємо йому шлях до папки, яку бажаємо перевірити. Якщо папка існує, метод поверне `true`, якщо ж її не існує -`false`.

## Глибоке вивчення

Метод `existsSync` перевіряє як папки, так і файли. Якщо потрібно перевірити лише існування папки, можна використати метод `statSync`, який буде повертати нам об'єкт статистики про файл або папку. За допомогою властивості `isDirectory()` ми можемо перевірити, чи є файл папкою. Також слід зазначити, що метод `existsSync` є синхронним, тому використання його може призвести до блокування і вплинути на продуктивність додатку.

## Дивись також
- [Документація TypeScript про модуль fs](https://www.typescriptlang.org/docs/handbook/fs.html)
- [Документація Node.js про модуль fs](https://nodejs.org/api/fs.html)
- [Стаття про перевірку існування файлів та папок в JavaScript](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-node-js)