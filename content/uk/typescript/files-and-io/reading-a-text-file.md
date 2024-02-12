---
title:                "Читання текстового файлу"
aliases:
- /uk/typescript/reading-a-text-file.md
date:                  2024-01-20T17:55:19.602058-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
(Що та чому?)
Читання текстового файлу — це процес отримання даних з файлу, збереженого на диску. Програмісти це роблять, щоб працювати з існуючою інформацією, аналізувати її чи трансформувати.

## How to:
(Як зробити:)
Щоб прочитати текстовий файл у TypeScript, можна використовувати модуль `fs` в Node.js. Нижче наведено приклад простого коду:

```TypeScript
import { readFileSync } from 'fs';

try {
  const data = readFileSync('example.txt', 'utf-8');
  console.log(data);
} catch (error) {
  console.error('Error reading file:', error);
}
```

Якщо в `example.txt` є текст "Привіт, світ!", вивід буде:

```
Привіт, світ!
```

## Deep Dive:
(Поглиблене занурення:)
Читання файлів було завжди основною частиною програмування. В Node.js модуль `fs` (filesystem) відповідає за операції з файлами, і він є доступним з самого початку. Існують асинхронні (`fs.readFile`) та синхронні (`fs.readFileSync`) методи. Асинхронні методи кращі для додатків, де важлива продуктивність, тому що вони не блокують event loop. Однак для простих скриптів або операцій, що вимагають послідовного виконання, можна використовувати синхронні методи.

## See Also:
(Дивіться також:)
- [Node.js fs Documentation](https://nodejs.org/api/fs.html) - офіційна документація модуля `fs`.
- [The Node.js Event Loop](https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/) - довідник, що пояснює, як працює event loop у Node.js.
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - посібник по TypeScript для глибшого розуміння мови.
