---
title:                "Проверка существования директории"
date:                  2024-01-28T23:55:31.051573-07:00
model:                 gpt-4-0125-preview
simple_title:         "Проверка существования директории"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Проверка существования директории заключается в убеждении, что папка действительно присутствует перед тем, как из неё читать или в неё писать. Программисты делают это, чтобы избежать ошибок, например, попытки сохранить файл в несуществующее место - это точно запрещено.

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
