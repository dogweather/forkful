---
title:                "Проверка существования директории"
aliases:
- ru/javascript/checking-if-a-directory-exists.md
date:                  2024-01-28T23:55:22.353136-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Проверка существования директории заключается в подтверждении того, что папка присутствует в указанном пути в файловой системе. Программисты делают это, чтобы избежать ошибок, например, попыток чтения из или записи в директорию, которой нет.

## Как это сделать:
В JavaScript (в среде Node.js) есть встроенный модуль под названием `fs`, который вы можете использовать для проверки существования директории. Вот быстрый пример:

```javascript
const fs = require('fs');
const path = './path/to/directory';

fs.access(path, fs.constants.F_OK, (err) => {
    if (err) {
        console.error(`${path} не существует`);
    } else {
        console.log(`${path} существует`);
    }
});
```

Пример вывода:

```
./path/to/directory существует
```

Или используя новый API `fs.promises` с async/await:

```javascript
const fs = require('fs').promises;

async function checkDirectoryExists(path) {
    try {
        await fs.access(path, fs.constants.F_OK);
        console.log(`${path} существует`);
    } catch {
        console.error(`${path} не существует`);
    }
}

checkDirectoryExists('./path/to/directory');
```

Пример вывода:

```
./path/to/directory не существует
```

## Подробнее
Исторически проверка файла или директории включала в себя использование `fs.stat` или `fs.existsSync`, но у них есть недостатки. `fs.stat` требует дополнительной логики для определения, является ли путь директорией, а `fs.existsSync` является синхронной, что может блокировать цикл событий в Node.js.

Альтернативой является использование API `fs.promises` или async/await для лучшей читаемости и чтобы ваша программа не блокировалась.

Одна из деталей реализации заключается в том, что `fs.access` проверяет только существование, а не возможность чтения или записи в директорию. Если это необходимо, с `fs.access` можно использовать другие флаги для проверки этих разрешений.

## Смотрите также
- Документация Node.js `fs`: [Модуль fs в Node.js](https://nodejs.org/api/fs.html)
- Больше об async/await: [Асинхронная функция](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Statements/async_function)
- Информация о флагах файловой системы: [Флаги файловой системы](https://nodejs.org/api/fs.html#file-system-flags)
