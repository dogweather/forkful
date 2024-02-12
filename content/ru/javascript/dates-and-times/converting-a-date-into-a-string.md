---
title:                "Преобразование даты в строку"
aliases:
- /ru/javascript/converting-a-date-into-a-string.md
date:                  2024-01-28T23:56:40.546907-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование даты в строку"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Преобразование даты в строку переводит объект Date в удобочитаемый текстовый формат, потому что людям больше нравится видеть "1 апреля 2023" вместо криптических временных меток. Программисты делают это для ясности в пользовательских интерфейсах и для форматирования дат перед сохранением или передачей по сети.

## Как это сделать:
JavaScript имеет встроенные методы для преобразования дат в строки. Вот как их использовать:

```javascript
const now = new Date();

// toLocaleString() - локальный формат
console.log(now.toLocaleString()); // '4/1/2023, 12:00:00 PM'

// toString() - стандартный формат
console.log(now.toString()); // 'Sat Apr 01 2023 12:00:00 GMT+0100 (Центральноевропейское стандартное время)'

// toISOString() - формат ISO (отлично подходит для баз данных/сети)
console.log(now.toISOString()); // '2023-04-01T11:00:00.000Z'
```

## Подробнее
Раньше преобразование даты в строку было путаницей — никаких стандартов, только куча пользовательских функций. К счастью, ECMAScript вступил в игру, стандартизировал объект Date в ES5 и добавил очень удобный `toISOString()` в ES5.1.

Альтернативы встроенным методам включают в себя библиотеки, такие как `moment.js` и `date-fns`, которые предлагают больше контроля и обработки часовых поясов, но они увеличивают размер вашего проекта.

Под капотом, когда вы вызываете метод преобразования даты в строку, JavaScript взаимодействует с локальными настройками системы и информацией о часовом поясе для генерации строкового вывода. В отличие от этого, `toISOString()` всегда возвращает время UTC ('Z' означает 'Zulu time' или нулевое смещение от UTC).

## Смотрите также
- [MDN Web Docs – Date](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 Формат даты и времени](https://www.iso.org/iso-8601-date-and-time-format.html)
- [date-fns](https://date-fns.org/)
