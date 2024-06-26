---
date: 2024-01-26 01:08:47.810950-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \ TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043B\u0435\u0433\
  \u043A\u043E \u0440\u0435\u0430\u043B\u0456\u0437\u0443\u0432\u0430\u0442\u0438\
  \ \u0431\u0430\u0437\u043E\u0432\u0435 \u043B\u043E\u0433\u0443\u0432\u0430\u043D\
  \u043D\u044F, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 \u043C\u0435\u0442\u043E\u0434\u0438 \u043A\u043E\u043D\u0441\
  \u043E\u043B\u0456, \u0430\u0431\u043E \u0456\u043D\u0442\u0435\u0433\u0440\u0443\
  \u0432\u0430\u0442\u0438 \u0431\u0456\u043B\u044C\u0448 \u0441\u043A\u043B\u0430\
  \u0434\u043D\u0435 \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0437\
  \u0430\u2026"
lastmod: '2024-03-13T22:44:48.877494-06:00'
model: gpt-4-1106-preview
summary: "\u0423 TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043B\
  \u0435\u0433\u043A\u043E \u0440\u0435\u0430\u043B\u0456\u0437\u0443\u0432\u0430\u0442\
  \u0438 \u0431\u0430\u0437\u043E\u0432\u0435 \u043B\u043E\u0433\u0443\u0432\u0430\
  \u043D\u043D\u044F, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0447\u0438 \u043C\u0435\u0442\u043E\u0434\u0438 \u043A\u043E\u043D\
  \u0441\u043E\u043B\u0456, \u0430\u0431\u043E \u0456\u043D\u0442\u0435\u0433\u0440\
  \u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u043B\u044C\u0448 \u0441\u043A\u043B\
  \u0430\u0434\u043D\u0435 \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F\
  \ \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A, \u0442\u0430\u043A\u0438\u0445 \u044F\
  \u043A `winston` \u0430\u0431\u043E `pino`."
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
weight: 17
---

## Як це робити:
У TypeScript ви можете легко реалізувати базове логування, використовуючи методи консолі, або інтегрувати більш складне логування за допомогою бібліотек, таких як `winston` або `pino`. Ось приклад базового використання `console.log` та більш складний з `winston`.

```TypeScript
// Базове логування у консолі
console.log('Інформація: Запуск програми...');
console.error('Помилка: Неможливо отримати дані.');

// Приклад виведення
// Інформація: Запуск програми...
// Помилка: Неможливо отримати дані.
```

Для більш надійного логування налаштуємо `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Сервер запущено!');
logger.warn('Попередження: Недостатньо місця на диску.');
logger.error('Не вдалося підключитися до бази даних.');

// Приклад виведення у файлі combined.log
// 2023-01-20 14:42:07 info: Сервер запущено!
// 2023-01-20 14:42:09 warn: Попередження: Недостатньо місця на диску.
// 2023-01-20 14:42:12 error: Не вдалося підключитися до бази даних.
```

## Детальніше:
Концепція логування в контексті обчислень сягає корінням початків програмування, де сам термін пішов від "логбука", системи ведення морських щоденників. Історично, програмні події часто реєструвалися у фізичних роздруківках або термінальних виходах, особливо під час головної комп'ютерної ери.

Плин часу до сьогодні, і ви маєте безліч інструментів та бібліотек на вашому розпорядженні, які задовольняють різні потреби у логуванні, від простих текстових файлів до складних систем керування логами. Альтернативи `winston` включають `pino`, який відрізняється високою продуктивністю, та `Bunyan`, що базується на JSON. При роботі з Node.js бібліотеки логування часто надають механізми потоків для перенаправлення логів у різні напрямки, підтримку ротації логів та налаштування форматерів.

Що стосується реалізації, повідомлення логів зазвичай містять часову мітку, рівень серйозності (такий як інформація, попередження, помилка) та саме повідомлення. Добрі практики логування рекомендують правильно класифікувати рівні логів, уникати конфіденційних даних у логах, та враховувати вплив на продуктивність у програмах з високим пропускним потоком.

## Дивіться також:
- [Winston - Логер для майже всього](https://www.npmjs.com/package/winston)
- [Pino - Логер Node.js з дуже низькими накладними витратами](https://www.npmjs.com/package/pino)
- [Найкращі практики логування Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [12-факторний додаток - Логи](https://12factor.net/logs)
