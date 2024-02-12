---
title:                "Чтение текстового файла"
aliases:
- /ru/typescript/reading-a-text-file/
date:                  2024-01-29T00:01:17.172600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение текстового файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Чтение текстового файла — это получение содержимого файла, структурированного как текст, доступный для чтения человеком. Программисты делают это для обработки или анализа данных, таких как чтение конфигурации, импорт данных или просто ввод контента для обработки приложением.

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
