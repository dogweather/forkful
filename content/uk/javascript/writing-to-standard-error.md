---
title:                "Запис до стандартної помилки"
aliases:
- uk/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:34:00.468534-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Запис у стандартний потік помилок (stderr) в JavaScript полягає в перенаправленні повідомлень про помилки або будь-якої критичної інформації до спеціального, окремого потоку, що особливо корисно в Unix-подібних середовищах для ведення журналу та відлагодження. Програмісти роблять це для розмежування нормального виведення програми від повідомлень про помилки, що дозволяє краще управляти виведенням і легше стежити за помилками.

## Як це зробити:
У Node.js запис у stderr можна здійснити за допомогою методу `console.error()` або шляхом прямого запису в `process.stderr`. Ось приклади, що демонструють обидва підходи:

```javascript
// Використання console.error()
console.error('Це повідомлення про помилку.');

// Прямий запис у process.stderr
process.stderr.write('Це ще одне повідомлення про помилку.\n');
```

Прикладний вивід для обох методів з'явиться в потоці stderr, не змішуючись з stdout:
```
Це повідомлення про помилку.
Це ще одне повідомлення про помилку.
```

Для більш складного або специфічного для додатку логування багато програмістів JavaScript використовують сторонні бібліотеки, такі як `winston` або `bunyan`. Ось швидкий приклад за допомогою `winston`:

Спочатку встановіть `winston` через npm:
```shell
npm install winston
```

Потім налаштуйте `winston` для логування помилок у stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Логування повідомлення про помилку
logger.error('Помилка залогована через winston.');
```

Ця налаштування забезпечує, що коли ви логуєте помилку за допомогою `winston`, вона спрямовується до stderr, допомагаючи підтримувати чітке розмежування між стандартним виведенням і виведенням помилок.
