---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:00.468534-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Node.js \u0437\u0430\u043F\u0438\u0441 \u0443 stderr \u043C\u043E\u0436\
  \u043D\u0430 \u0437\u0434\u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043C\u0435\u0442\u043E\
  \u0434\u0443 `console.error()` \u0430\u0431\u043E \u0448\u043B\u044F\u0445\u043E\
  \u043C \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0437\u0430\u043F\u0438\u0441\
  \u0443 \u0432 `process.stderr`. \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\
  \u0430\u0434\u0438,\u2026"
lastmod: '2024-03-13T22:44:50.021930-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Node.js \u0437\u0430\u043F\u0438\u0441 \u0443 stderr \u043C\u043E\
  \u0436\u043D\u0430 \u0437\u0434\u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043C\u0435\u0442\
  \u043E\u0434\u0443 `console.error()` \u0430\u0431\u043E \u0448\u043B\u044F\u0445\
  \u043E\u043C \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u0437\u0430\u043F\u0438\
  \u0441\u0443 \u0432 `process.stderr`."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

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
