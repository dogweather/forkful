---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:59:17.302907-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Перевіряємо існування директорії, щоб уникнути помилок під час читання, запису чи створення файлів. Це робимо, щоб забезпечити надійність та коректність програми.

## How to: (Як це зробити:)
```TypeScript
import * as fs from 'fs';

// Асинхронна функція для перевірки директорії
async function checkDirectoryExists(path: string): Promise<Boolean> {
  try {
    await fs.promises.access(path, fs.constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

// Приклад використання
(async () => {
  const directoryPath = './myDirectory';
  const exists = await checkDirectoryExists(directoryPath);
  console.log(`Directory "${directoryPath}" exists: ${exists}`);
})();
```

Sample Output:
```
Directory "./myDirectory" exists: true
```
або, якщо директорії не існує:
```
Directory "./myDirectory" exists: false
```

## Deep Dive (Детальний Розгляд)
Перевірка існування директорій у TypeScript - це процес, заснований на Node.js file system (fs) модулі. Функція `fs.access()` перевіряє доступність файлу чи директорії. Параметр `fs.constants.F_OK` використовується, щоб перевірити існування шляху.

Альтернативою асинхронної перевірки є синхронний метод `fs.existsSync(path)`, але він блокує event loop, тому краще використовувати асинхронні функції для неблокуючих операцій.

Раніше, до ES7, розробники часто використовували пакети, такі як `fs-extra`, або колбеки в `fs.exists()`, але це застарілі підходи в сучасному Node.js.

## See Also (Дивіться також)
- Node.js fs module docs: https://nodejs.org/api/fs.html
- Understanding filesystem in Node.js: https://nodejs.dev/learn/the-nodejs-filesystem-module
- Working with Promises: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
