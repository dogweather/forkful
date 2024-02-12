---
title:                "Запись в стандартный поток ошибок"
aliases:
- /ru/javascript/writing-to-standard-error.md
date:                  2024-01-29T00:06:04.912048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Запись в стандартный поток ошибок (stderr) представляет собой вывод текста в поток ошибок. Это позволяет отделить обычный вывод (stdout) от ошибок, упрощая отладку и анализ логов.

## Как это сделать:

```javascript
// Запись простого сообщения об ошибке в stderr
console.error('Ошибка: Что-то пошло не так');

// Пример с форматированным выводом
const errorCode = 404;
console.error(`Ошибка: Страница не найдена - Код ${errorCode}`);
```

Пример вывода:
```
Ошибка: Что-то пошло не так
Ошибка: Страница не найдена - Код 404
```

## Подробнее
Исторически в системах, подобных Unix, делается различие между стандартным выводом и стандартным выводом ошибок, чтобы позволить отдельно обрабатывать обычные сообщения и сообщения об ошибках. В то время как `console.log` в Javascript записывает в stdout, `console.error` специально записывает в stderr.
Альтернативы для записи в stderr включают использование `process.stderr.write()`, который не добавляет символ новой строки в конце, в отличие от `console.error`.
С точки зрения реализации, при написании скриптов Node.js, вывод в `console.error()` может быть перенаправлен отдельно от `console.log()` при выполнении скрипта из командной строки, что может быть полезно для записи ошибок в отдельный файл.

## Смотрите также
- Документация MDN по Console: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Документация Node.js по `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Объяснение stdout против stderr: https://www.jstor.org/stable/25860673
