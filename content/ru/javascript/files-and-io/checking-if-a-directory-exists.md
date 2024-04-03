---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:22.353136-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 JavaScript (\u0432 \u0441\u0440\u0435\u0434\u0435 Node.js)\
  \ \u0435\u0441\u0442\u044C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\
  \u0439 \u043C\u043E\u0434\u0443\u043B\u044C \u043F\u043E\u0434 \u043D\u0430\u0437\
  \u0432\u0430\u043D\u0438\u0435\u043C `fs`, \u043A\u043E\u0442\u043E\u0440\u044B\u0439\
  \ \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0434\u043B\u044F \u043F\u0440\u043E\
  \u0432\u0435\u0440\u043A\u0438 \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u043E\u0432\
  \u0430\u043D\u0438\u044F\u2026"
lastmod: '2024-03-13T22:44:45.786495-06:00'
model: gpt-4-0125-preview
summary: "\u0412 JavaScript (\u0432 \u0441\u0440\u0435\u0434\u0435 Node.js) \u0435\
  \u0441\u0442\u044C \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0439\
  \ \u043C\u043E\u0434\u0443\u043B\u044C \u043F\u043E\u0434 \u043D\u0430\u0437\u0432\
  \u0430\u043D\u0438\u0435\u043C `fs`, \u043A\u043E\u0442\u043E\u0440\u044B\u0439\
  \ \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0434\u043B\u044F \u043F\u0440\u043E\
  \u0432\u0435\u0440\u043A\u0438 \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u043E\u0432\
  \u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0438\u0438\
  ."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

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
