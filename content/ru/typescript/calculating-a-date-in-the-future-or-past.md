---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:55:44.597272-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Расчет будущей или прошедшей даты заключается в изменении текущей даты, чтобы узнать, какой день будет, например, через 10 дней, или какой день был 10 дней назад. Программисты делают это для таких функций, как сроки годности, планирование событий или вычисление разницы во времени.

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
