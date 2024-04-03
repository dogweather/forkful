---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:32.507953-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u0442\u0435\
  \u043A\u0443\u0449\u0443\u044E \u0434\u0430\u0442\u0443 \u0432 TypeScript."
lastmod: '2024-03-13T22:44:44.608030-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C \u0442\u0435\u043A\
  \u0443\u0449\u0443\u044E \u0434\u0430\u0442\u0443 \u0432 TypeScript."
title: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0443\
  \u0449\u0435\u0439 \u0434\u0430\u0442\u044B"
weight: 29
---

## Как это сделать:
Вот как вы можете получить текущую дату в TypeScript:

```typescript
// Получить текущие дату и время
const now = new Date();

// Вывести в консоль
console.log(now);
```

Пример вывода может выглядеть так:

```
2023-04-01T12:34:56.789Z
```

Но если вам нужна только дата без времени:

```typescript
const today = new Date().toISOString().split('T')[0];

console.log(today);
```

И это даст вам:

```
2023-04-01
```

## Глубокое погружение
Объект `Date` JavaScript - это то, с чем вы работаете в TypeScript для дат и времени. Он существует с ранних дней, созданный как часть ECMAScript 1 в 1997 году. Альтернативы нативному `Date` включают в себя библиотеки, такие как `moment.js` или `date-fns`, которые предлагают больше функций и лучший парсинг.

Под капотом, `new Date()` возвращает количество миллисекунд с Unix Epoch (1 января 1970 года). Вот как компьютеры отслеживают время. Часовые пояса могут быть каверзными, особенно когда вам нужно отображать даты пользователям по всему миру. По умолчанию, `new Date()` будет использовать местное время системы. Метод `toISOString()` преобразует дату в координированное универсальное время (UTC) и форматирует его как строку ISO.

## Смотрите также
- MDN Web Docs по теме `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Обработка часовых поясов в JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString
