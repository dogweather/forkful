---
title:                "Получение текущей даты"
aliases:
- /ru/typescript/getting-the-current-date.md
date:                  2024-01-28T23:58:32.507953-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Получение текущей даты в вашем коде означает захватить настоящий момент до дня. Программисты делают это для того, чтобы ставить временные метки на события, управлять расписанием и отслеживать продолжительность или интервалы.

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
