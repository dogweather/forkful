---
title:                "Письмо на стандартну помилку"
html_title:           "TypeScript: Письмо на стандартну помилку"
simple_title:         "Письмо на стандартну помилку"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що та чому?

Запис до одержуваної помилкою є процесом, коли розробники використовують спеціальний потік виводу у своїй програмі для відображення помилок або повідомлень про проблеми. Це дозволяє їм легше виявляти та усувати помилки у своєму коді, спрощує процес налагодження та полегшує життя програмістів.

## Як це зробити?

```typescript
import { write } = require('standard-error');

function add(x: number, y: number): number {
  if (typeof x !== 'number' || typeof y !== 'number') {
    write('Недопустимі параметри. Очікувані типи: number, number.');
    return NaN;
  }
  return x + y;
}
```

Приклад виводу:
```
Недопустимі параметри. Очікувані типи: number, number.
```

## Глибока підводка

Написання до одержуваної помилкою використовувалося вже давно у різних мовах програмування, і TypeScript не є винятком. Це корисний інструмент для помилкової знахідки та налагодження, але існують альтернативи, такі як використання debug-попап-інструментів або просто використовувати console.log() для виводу до потоку помилки.

Щоб записати до одержуваної помилкою у TypeScript, ви можете використовувати бібліотеку "standard-error" або відповідну функцію у побудованих вбудованих об'єктів NodeJS.

## Дивись далі

- [Документація запису до одержуваної помилкою в TypeScript] (https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#type-widening)
- [Виведення до потоків помилок за допомогою NodeJS] (https://nodejs.org/api/process.html#process_event_uncaughtexception)