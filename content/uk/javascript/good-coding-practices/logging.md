---
date: 2024-01-26 01:06:59.579793-07:00
description: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F (\u0432\u0435\
  \u0434\u0435\u043D\u043D\u044F \u0436\u0443\u0440\u043D\u0430\u043B\u0443 \u043F\
  \u043E\u0434\u0456\u0439) \u2013 \u0446\u0435 \u0441\u0432\u043E\u0454\u0440\u0456\
  \u0434\u043D\u0438\u0439 \u0449\u043E\u0434\u0435\u043D\u043D\u0438\u043A \u0434\
  \u043B\u044F \u0432\u0430\u0448\u043E\u0457 \u0430\u043F\u043B\u0456\u043A\u0430\
  \u0446\u0456\u0457, \u044F\u043A\u0438\u0439 \u0437\u0430\u043F\u0438\u0441\u0443\
  \u0454 \u043F\u043E\u0434\u0456\u0457, \u043F\u043E\u043C\u0438\u043B\u043A\u0438\
  \ \u0442\u0430 \u0456\u043D\u0448\u0456 \u0437\u043D\u0430\u0447\u0438\u043C\u0456\
  \ \u0434\u0456\u0457, \u0449\u043E \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u044E\
  \u0442\u044C\u0441\u044F \u043F\u0456\u0434 \u0447\u0430\u0441\u2026"
lastmod: '2024-03-13T22:44:50.004846-06:00'
model: gpt-4-1106-preview
summary: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F (\u0432\u0435\u0434\
  \u0435\u043D\u043D\u044F \u0436\u0443\u0440\u043D\u0430\u043B\u0443 \u043F\u043E\
  \u0434\u0456\u0439) \u2013 \u0446\u0435 \u0441\u0432\u043E\u0454\u0440\u0456\u0434\
  \u043D\u0438\u0439 \u0449\u043E\u0434\u0435\u043D\u043D\u0438\u043A \u0434\u043B\
  \u044F \u0432\u0430\u0448\u043E\u0457 \u0430\u043F\u043B\u0456\u043A\u0430\u0446\
  \u0456\u0457, \u044F\u043A\u0438\u0439 \u0437\u0430\u043F\u0438\u0441\u0443\u0454\
  \ \u043F\u043E\u0434\u0456\u0457, \u043F\u043E\u043C\u0438\u043B\u043A\u0438 \u0442\
  \u0430 \u0456\u043D\u0448\u0456 \u0437\u043D\u0430\u0447\u0438\u043C\u0456 \u0434\
  \u0456\u0457, \u0449\u043E \u0432\u0456\u0434\u0431\u0443\u0432\u0430\u044E\u0442\
  \u044C\u0441\u044F \u043F\u0456\u0434 \u0447\u0430\u0441\u2026"
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
---

{{< edit_this_page >}}

## Що і чому?
Логування (ведення журналу подій) – це своєрідний щоденник для вашої аплікації, який записує події, помилки та інші значимі дії, що відбуваються під час роботи програмного забезпечення. Програмісти роблять це не лише для розуміння того, що відбувається «під капотом» в реальному часі, але й для створення історичного запису, який є критично важливим для відладки, аудиту та оптимізації продуктивності.

## Як це зробити:
З коробки JavaScript пропонує простий спосіб виведення повідомлень у консоль:

```javascript
console.log('Це буде виведено в консолі');

// Вивід:
// Це буде виведено в консолі
```

Але додатки реального світу потребують більшого, ніж просто друкування повідомлень у консоль. Бібліотеки на кшталт Winston чи Pino можна використовувати для ефективного управління логами:

```javascript
// Використання Winston для розширеного логування
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Привіт, це подія логування з Winston');
// Цей лог записано у 'combined.log' у форматі JSON
```

Приклад виводу у `combined.log`:

```json
{"message":"Привіт, це подія логування з Winston","level":"info"}
```

## Поглиблений аналіз
Логування було невід'ємною частиною з самого початку від часів обчислювальної техніки; оператори систем аналізували логи, щоб зрозуміти продуктивність системи та діагностувати проблеми. На сьогоднішній день у сучасній розробці ми перейшли від простих файлів логів до структурованих і пошукових систем управління логами.

Альтернативи логуванню у консоль чи у файл у JavaScript включають використання хмарних послуг логування, таких як Loggly, Datadog або ELK Stack (Elasticsearch, Logstash, Kibana), які можуть агрегувати логи з багатьох джерел, пропонують інструменти візуалізації та розширений аналітичний функціонал.

При реалізації логування слід враховувати наступне:
- **Рівень деталізації**: Включаючи debug, info, warning, error та critical.
- **Продуктивність**: Надмірне логування може впливати на продуктивність аплікацій.
- **Безпека**: Будьте обачні при логуванні конфіденційної інформації.
- **Формат**: Структуровані логи (такі як JSON) спрощують пошук та аналіз логів.
- **Політика зберігання**: Старі логи потрібно архівувати або видаляти для економії місця.

Практична стратегія логування визначає, що логувати, куди це логувати та як довго зберігати це, знаходячи баланс між інформаційним прозором проти продуктивності та конфіденційності.

## Дивіться також
Ознайомтеся з цими ресурсами для поглибленого вивчення:
- [GitHub-репозиторій Winston](https://github.com/winstonjs/winston): для детального використання та власних транспортів.
- [Pino - дуже низької навантаження Node.js логер](https://github.com/pinojs/pino): легке рішення для логування.
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): для основної інформації про логування в браузері.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): потужна трійця для управління логами.
- [Логування додатків за принципом 12 факторів](https://12factor.net/logs): найкращі практики логування додатків.
