---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:08.601202-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 TypeScript \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435\
  \ \u0437\u0430\u043F\u0438\u0441\u044B\u0432\u0430\u0442\u044C \u0432 `stderr` \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E `console.error` \u0438\u043B\u0438\
  \ `process.stderr.write`. \u0412\u043E\u0442 \u043F\u0440\u0438\u043C\u0435\u0440\
  \u044B \u043E\u0431\u043E\u0438\u0445 \u043C\u0435\u0442\u043E\u0434\u043E\u0432\
  ."
lastmod: '2024-03-13T22:44:44.618837-06:00'
model: gpt-4-0125-preview
summary: "\u0412 TypeScript \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0437\
  \u0430\u043F\u0438\u0441\u044B\u0432\u0430\u0442\u044C \u0432 `stderr` \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E `console.error` \u0438\u043B\u0438 `process.stderr.write`."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

## Как это сделать:
В TypeScript вы можете записывать в `stderr` с помощью `console.error` или `process.stderr.write`. Вот примеры обоих методов:

```TypeScript
console.error("Это сообщение об ошибке в stderr");

process.stderr.write("Это еще одно сообщение об ошибке в stderr\n");
```

Пример вывода для обеих строк:

```
Это сообщение об ошибке в stderr
Это еще одно сообщение об ошибке в stderr
```

## Глубокое погружение
Исторически разделение `stdout` и `stderr` позволяло пользователям Unix направлять вывод и ошибки в различные места. Вы могли регистрировать ошибки для анализа, имея при этом чистые выходные данные. Альтернативы прямой записи в `stderr` включают логгирующие библиотеки или фреймворки, предлагающие больше контроля и функций. С точки зрения реализации, `console.error` является оберткой вокруг `process.stderr.write` с дополнительными возможностями форматирования, поэтому использование `console.error` обычно более удобно для простых сообщений.

## Смотрите также
- Документация Node.js по консоли: https://nodejs.org/api/console.html
- Стандартные потоки процесса Node.js: https://nodejs.org/api/process.html#process_process_stderr
- Обсуждение `console.error` против `process.stderr.write`: https://stackoverflow.com/questions/4976466/difference-between-process-stdout-write-and-console-log-in-node-js
