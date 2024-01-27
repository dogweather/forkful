---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Що це та навіщо?)
Запис у стандартний потік помилок (stderr) дозволяє відділити повідомлення про помилки від звичайного виводу програми. Програмісти це використовують для діагностики та логування, задля легшого відстеження проблем.

## How to: (Як це зробити:)
```TypeScript
// Пишемо у стандартний потік помилок
process.stderr.write('Це повідомлення про помилку.\n');

// Використання console.error для запису у stderr
console.error('Помилка: Неверифікований вхід.');

// Результат:
// Це повідомлення про помилку.
// Помилка: Неверифікований вхід.
```

## Deep Dive (Поглиблений аналіз)
Стандартні потоки введення/виводу започатковані ще у Unix, їх тріо: stdin, stdout, і stderr. Запис у stderr використовують, коли потрібно відокремити помилки і логи від основних даних виводу. Альтернативи включають логування у файл чи за допомогою зовнішніх систем, але stderr залишається ефективним для оперативних повідомлень і дебагінга. В TypeScript, як надмножини JavaScript, stderr можна особливо легко використовувати через глобальний об'єкт `process` у Node.js або використовуючи `console.error()`.

## See Also (Дивіться також)
- [Node.js Process](https://nodejs.org/api/process.html#process_process_stderr) - докладніше про глобальний об'єкт `process` в Node.js.
- [Console.error](https://developer.mozilla.org/docs/Web/API/console/error) - документація MDN про `console.error()`.
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams) - історія стандартних потоків у Unix.
