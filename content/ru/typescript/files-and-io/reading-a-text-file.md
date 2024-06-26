---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:17.172600-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u0440\u043E\u0447\
  \u0438\u0442\u0430\u0435\u043C \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439\
  \ \u0444\u0430\u0439\u043B \u043D\u0430 TypeScript \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043C\u043E\u0434\u0443\
  \u043B\u044F `fs/promises` \u0432 Node.js. \u0421\u0434\u0435\u043B\u0430\u0435\u043C\
  \ \u044D\u0442\u043E\u0442 \u043F\u0440\u0438\u043C\u0435\u0440 \u043F\u0440\u043E\
  \u0441\u0442\u044B\u043C: \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0435\u043C\
  \u2026"
lastmod: '2024-03-13T22:44:44.620633-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u0440\u043E\u0447\u0438\
  \u0442\u0430\u0435\u043C \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0439\
  \ \u0444\u0430\u0439\u043B \u043D\u0430 TypeScript \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u043C\u043E\u0434\u0443\
  \u043B\u044F `fs/promises` \u0432 Node.js."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

## Как это сделать:
Давайте прочитаем текстовый файл на TypeScript с использованием модуля `fs/promises` в Node.js. Сделаем этот пример простым: прочитаем файл с именем `example.txt` и выведем его содержимое в лог.

```typescript
import { readFile } from 'fs/promises';

async function readTextFile(filePath: string) {
  try {
    const data = await readFile(filePath, 'utf8');
    console.log(data);
  } catch (error) {
    console.error(`Ошибка чтения файла с диска: ${error}`);
  }
}

readTextFile('./example.txt');
```

Пример вывода:
```
Привет, это содержимое файла!
```

## Подробнее
Исторически чтение файлов в Node.js базировалось на использовании обратных вызовов (callbacks), что могло привести к явлению, известному как «ад обратных вызовов». С появлением Promises и `async/await` этот процесс стал значительно более структурированным.

Помимо `fs/promises`, существует старый модуль `fs`, который по-прежнему работает с паттернами обратных вызовов. Также существует возможность использовать обработку потоков с помощью `fs.createReadStream()`, что полезно для больших файлов из-за меньшего потребления памяти.

С точки зрения реализации, доступ к файловой системе является операцией ввода/вывода и по своей природе медленнее операций в памяти. Вот почему важны асинхронные модели кодирования — они помогают предотвращать блокировку главного потока и позволяют Node.js продолжать обрабатывать другие задачи.

## Смотрите также
Для более глубокого изучения файловой системы Node.js:
- Документация по fs Node.js: https://nodejs.org/api/fs.html
- Понимание `fs/promises`: https://nodejs.org/dist/latest/docs/api/fs.html#filehandlepromises
- Потоковое чтение файлов: https://nodejs.org/api/stream.html#stream
Для ресурсов, специфичных для TypeScript:
- Глубокое погружение в TypeScript: https://basarat.gitbook.io/typescript/
- Руководство по TypeScript: https://www.typescriptlang.org/docs/handbook/intro.html
