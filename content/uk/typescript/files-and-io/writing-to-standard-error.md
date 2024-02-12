---
title:                "Запис до стандартної помилки"
aliases:
- /uk/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:34:57.995809-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
У TypeScript запис у стандартний потік помилок (stderr) - це процес відправлення повідомлень про помилки або логів безпосередньо до потоку виведення помилок середовища (наприклад, консолі в node.js або веб-браузері). Це є важливим для діагностики проблем без втручання в стандартний вихід (stdout), який зазвичай використовується для даних програми, забезпечуючи ефективне і згуртоване управління обробкою помилок та логуванням.

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
