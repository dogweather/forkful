---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:57.995809-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : TypeScript, \u0431\u0443\u0434\u0443\u0447\u0438 \u043D\u0430\u0434\u043C\u043D\
  \u043E\u0436\u0438\u043D\u043E\u044E JavaScript, \u043F\u043E\u043A\u043B\u0430\u0434\
  \u0430\u0454\u0442\u044C\u0441\u044F \u043D\u0430 \u0431\u0430\u0437\u043E\u0432\
  \u0435 JS \u0441\u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0435 \u0432\u0438\
  \u043A\u043E\u043D\u0430\u043D\u043D\u044F (\u044F\u043A-\u043E\u0442 Node.js) \u0434\
  \u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0443 \u0432 stderr. \u041E\u0441\u044C\
  \ \u044F\u043A \u0432\u0438\u2026"
lastmod: '2024-03-13T22:44:48.893857-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u0431\u0443\u0434\u0443\u0447\u0438 \u043D\u0430\u0434\u043C\
  \u043D\u043E\u0436\u0438\u043D\u043E\u044E JavaScript, \u043F\u043E\u043A\u043B\u0430\
  \u0434\u0430\u0454\u0442\u044C\u0441\u044F \u043D\u0430 \u0431\u0430\u0437\u043E\
  \u0432\u0435 JS \u0441\u0435\u0440\u0435\u0434\u043E\u0432\u0438\u0449\u0435 \u0432\
  \u0438\u043A\u043E\u043D\u0430\u043D\u043D\u044F (\u044F\u043A-\u043E\u0442 Node.js)\
  \ \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0443 \u0432 stderr."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

## Як це зробити:
TypeScript, будучи надмножиною JavaScript, покладається на базове JS середовище виконання (як-от Node.js) для запису в stderr. Ось як ви можете це зробити безпосередньо:

```typescript
console.error("Це повідомлення про помилку.");
```

Приклад виводу в stderr:
```
Це повідомлення про помилку.
```

У середовищі Node.js ви також можете використовувати метод `process.stderr.write()` для більш низькорівневого запису:

```typescript
process.stderr.write("Повідомлення про помилку низького рівня.\n");
```

Приклад виводу в stderr:
```
Повідомлення про помилку низького рівня.
```

Для більш структурованого логування помилок ви можете використовувати популярні сторонні бібліотеки, такі як `winston` або `pino`. Ось як логувати помилки за допомогою `winston`:

Спочатку встановіть `winston`:

```bash
npm install winston
```

Потім використовуйте його у вашому файлі TypeScript:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Помилка залогована за допомогою winston.');
```

Це запише помилку до консолі та до файлу під назвою `error.log`. Пам'ятайте, пишучи в файли, важливо керувати дозволами на файли та ротацією, щоб запобігти проблемам, пов'язаним з використанням дискового простору.
