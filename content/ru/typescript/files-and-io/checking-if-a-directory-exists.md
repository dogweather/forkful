---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:31.051573-07:00
description: "\u041A\u0430\u043A: \u0412 TypeScript \u043E\u0431\u044B\u0447\u043D\
  \u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F\
  \ \u043C\u043E\u0434\u0443\u043B\u044C `fs` \u0438\u0437 Node.js \u0434\u043B\u044F\
  \ \u043F\u0440\u043E\u0432\u0435\u0440\u043A\u0438 \u0434\u0438\u0440\u0435\u043A\
  \u0442\u043E\u0440\u0438\u0438. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C \u044D\u0442\u043E."
lastmod: '2024-03-13T22:44:44.615263-06:00'
model: gpt-4-0125-preview
summary: "\u0412 TypeScript \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0441\u044F \u043C\u043E\u0434\u0443\u043B\
  \u044C `fs` \u0438\u0437 Node.js \u0434\u043B\u044F \u043F\u0440\u043E\u0432\u0435\
  \u0440\u043A\u0438 \u0434\u0438\u0440\u0435\u043A\u0442\u043E\u0440\u0438\u0438."
title: "\u041F\u0440\u043E\u0432\u0435\u0440\u043A\u0430 \u0441\u0443\u0449\u0435\u0441\
  \u0442\u0432\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0438\u0440\u0435\u043A\u0442\
  \u043E\u0440\u0438\u0438"
weight: 20
---

## Как:
В TypeScript обычно используется модуль `fs` из Node.js для проверки директории. Вот простой способ сделать это:

```typescript
import { existsSync } from 'fs';

// Проверяем существование директории
const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log(`Да, она есть!`);
} else {
  console.log(`Нет, её не существует.`);
}
```

Вывод зависит от существования директории:
```
Да, она есть!
// или
Нет, её не существует.
```

## Подробнее
Раньше люди использовали асинхронный `fs.exists`, но его признали устаревшим из-за того, что он имел склонность вызывать ошибки в коде, такие как условия гонки при проверке, а затем действии. `existsSync` проще и исключает путаницу с обратными вызовами.

Что касается альтернатив, методы `fs.statSync` или `fs.accessSync` тоже могут справиться с задачей, но требуют чуть больше кода:

```typescript
import { statSync } from 'fs';

try {
  const stats = statSync(directoryPath);
  if (stats.isDirectory()) {
    console.log('Действительно существует.');
  }
} catch (error) {
  if (error.code === 'ENOENT') {
    console.log('Нет, нигде не найдено.');
  }
}
```

И `statSync`, и `accessSync` выбрасывают ошибки, если путь не существует, так что вам нужно будет обработать это.

Используя TypeScript, помните, что эти методы поступают из Node.js, а не из TypeScript как такового. А роль TypeScript? В основном, он просто предоставляет типы и убеждается, что вы правильно используете методы.

## Смотрите также
- Документация по файловой системе Node.js: https://nodejs.org/api/fs.html
- Руководство по TypeScript: https://www.typescriptlang.org/docs/handbook/intro.html
- Обработка ошибок в Node.js: https://nodejs.org/en/knowledge/errors/what-are-the-error-conventions/
