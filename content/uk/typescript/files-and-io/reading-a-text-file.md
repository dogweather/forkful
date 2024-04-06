---
date: 2024-01-20 17:55:19.602058-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u0429\
  \u043E\u0431 \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u0438 \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u0438\u0439 \u0444\u0430\u0439\u043B \u0443 TypeScript,\
  \ \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043C\u043E\u0434\u0443\u043B\u044C\
  \ `fs` \u0432 Node.js. \u041D\u0438\u0436\u0447\u0435 \u043D\u0430\u0432\u0435\u0434\
  \u0435\u043D\u043E \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u0433\u043E \u043A\u043E\u0434\u0443."
lastmod: '2024-04-05T21:53:49.121036-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) \u0429\u043E\u0431\
  \ \u043F\u0440\u043E\u0447\u0438\u0442\u0430\u0442\u0438 \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u0438\u0439 \u0444\u0430\u0439\u043B \u0443 TypeScript, \u043C\
  \u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u043C\u043E\u0434\u0443\u043B\u044C `fs` \u0432\
  \ Node.js."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
