---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:44.597272-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0418\
  \u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438 \u0443\u043F\u0440\u0430\
  \u0432\u043B\u0435\u043D\u0438\u0435 \u0434\u0430\u0442\u0430\u043C\u0438 \u0432\
  \ JavaScript\u2014\u0438, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\
  \u0432\u0435\u043D\u043D\u043E, \u0432 TypeScript\u2014\u0431\u044B\u043B\u043E\
  \ \u0437\u0430\u0442\u0440\u0443\u0434\u043D\u0435\u043D\u043E \u0438\u0437-\u0437\
  \u0430 \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E\u0441\u0442\u0435\u0439\
  \ \u043E\u0431\u044A\u0435\u043A\u0442\u0430 Date \u0438 \u0447\u0430\u0441\u043E\
  \u0432\u044B\u0445 \u043F\u043E\u044F\u0441\u043E\u0432.\u2026"
lastmod: '2024-04-05T22:50:58.183127-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438 \u0443\
  \u043F\u0440\u0430\u0432\u043B\u0435\u043D\u0438\u0435 \u0434\u0430\u0442\u0430\u043C\
  \u0438 \u0432 JavaScript\u2014\u0438, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\
  \u0441\u0442\u0432\u0435\u043D\u043D\u043E, \u0432 TypeScript\u2014\u0431\u044B\u043B\
  \u043E \u0437\u0430\u0442\u0440\u0443\u0434\u043D\u0435\u043D\u043E \u0438\u0437\
  -\u0437\u0430 \u043E\u0441\u043E\u0431\u0435\u043D\u043D\u043E\u0441\u0442\u0435\
  \u0439 \u043E\u0431\u044A\u0435\u043A\u0442\u0430 Date \u0438 \u0447\u0430\u0441\
  \u043E\u0432\u044B\u0445 \u043F\u043E\u044F\u0441\u043E\u0432."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

## Как сделать:
```TypeScript
// Получить текущую дату
const today: Date = new Date();

// Рассчитать 10 дней в будущем
const tenDaysLater: Date = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));
console.log(`Через десять дней будет: ${tenDaysLater.toDateString()}`);

// Рассчитать 10 дней в прошлом
const tenDaysBefore: Date = new Date(today.getTime() - (10 * 24 * 60 * 60 * 1000));
console.log(`Десять дней назад было: ${tenDaysBefore.toDateString()}`);
```
Пример вывода:
```
Через десять дней будет: Вс апр 23 2023
Десять дней назад было: Ср апр 03 2023
```

## Подробнее
Исторически управление датами в JavaScript—и, соответственно, в TypeScript—было затруднено из-за особенностей объекта Date и часовых поясов. Альтернативные библиотеки, такие как Moment.js и date-fns, предложили абстракции для обработки этой сложности. С появлением ES6 улучшилась поддержка интернационализации благодаря API `Intl`, который также может использовать TypeScript.

При расчете дат следите за изменениями, связанными с переходом на летнее время и секундами высокосных секунд. Такие изменения могут нарушить простые расчеты, например, добавление 24 часов к дате. Также всегда учитывайте локаль и часовой пояс пользователя при отображении рассчитанных дат.

Для широкой совместимости и гибкости вы можете выбрать библиотеки вроде `date-fns` или `Luxon`, которые модульны и могут быть отличным вариантом для сложных задач. Например, с помощью `date-fns` можно легко добавить дни:

```TypeScript
import { addDays } from 'date-fns';

const result = addDays(new Date(2023, 3, 13), 10); // 13 апреля 2023 + 10 дней
console.log(result.toDateString());
```

Они также обрабатывают краевые случаи и проблемы с часовыми поясами, значительно облегчая арифметику дат.

## Смотрите также
- [Справочник по датам MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Библиотека date-fns](https://date-fns.org/)
- [Документация Luxon](https://moment.github.io/luxon/#/)
- [Официальная документация TypeScript](https://www.typescriptlang.org/docs/)
