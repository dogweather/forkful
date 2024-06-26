---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:44.361744-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041E\u0431\u044A\u0435\u043A\u0442 `Date` \u0432 JavaScript - \u0432\
  \u0430\u0448 \u043B\u0443\u0447\u0448\u0438\u0439 \u043F\u043E\u043C\u043E\u0449\
  \u043D\u0438\u043A \u0434\u043B\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\
  \u044F\u0446\u0438\u0439 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438. \u0414\u0430\
  \u0432\u0430\u0439\u0442\u0435 \u043F\u043E\u0438\u0433\u0440\u0430\u0435\u043C\
  \ \u0441 \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u043C\u0438 \u043F\
  \u0440\u0438\u043C\u0435\u0440\u0430\u043C\u0438."
lastmod: '2024-03-13T22:44:45.784671-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0431\u044A\u0435\u043A\u0442 `Date` \u0432 JavaScript - \u0432\u0430\
  \u0448 \u043B\u0443\u0447\u0448\u0438\u0439 \u043F\u043E\u043C\u043E\u0449\u043D\
  \u0438\u043A \u0434\u043B\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\
  \u0446\u0438\u0439 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

## Как это сделать:
Объект `Date` в JavaScript - ваш лучший помощник для манипуляций с датами. Давайте поиграем с несколькими примерами:

```javascript
// Сегодняшняя дата
let today = new Date();
console.log(today); // Выводит текущую дату и время

// Рассчитать дату через 7 дней в будущем
let nextWeek = new Date();
nextWeek.setDate(today.getDate() + 7);
console.log(nextWeek); // Выводит дату через 7 дней от сегодняшнего дня

// Рассчитать дату 30 дней назад
let lastMonth = new Date();
lastMonth.setDate(today.getDate() - 30);
console.log(lastMonth); // Выводит дату за 30 дней до сегодняшнего дня

// Установить дату на 1 год в будущее
let nextYear = new Date();
nextYear.setFullYear(today.getFullYear() + 1);
console.log(nextYear); // Выводит дату на тот же день в следующем году
```
Результаты зависят от времени запуска этого кода, так как `today` — это ваша текущая дата-время.

## Подробнее
До того как в JavaScript появились встроенные функции для работы с датами, программистам приходилось вручную рассчитывать даты, учитывая различия в длине месяцев, високосные годы и часовые пояса — настоящая боль! С `Date` многие из этих хлопот исчезают.

Альтернативы нативному объекту `Date` включают в себя библиотеки, такие как `moment.js` и `date-fns`, которые предлагают более богатый синтаксис и решают такие проблемы, как ошибки перехода на летнее время.

Когда вы рассчитываете даты, помните: `Date` считает месяцы с 0 (январь) по 11 (декабрь), а не с 1-12. И не забудьте учитывать високосные годы, когда работаете с датами февраля.

## Смотрите также
- Документация MDN о Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js: https://momentjs.com/
- date-fns: https://date-fns.org/
