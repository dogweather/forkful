---
title:                "Сравнение двух дат"
date:                  2024-01-28T23:55:31.059585-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Сравнение двух дат включает в себя определение их хронологических отношений — являются ли они одинаковыми, одна раньше или, может быть, позже другой? Программисты делают это для планирования событий, сортировки временных шкал и проверки продолжительности.

## Как это сделать:

Давайте сравним некоторые даты:

```TypeScript
const date1 = new Date('2023-04-01T00:00:00Z');
const date2 = new Date('2023-04-02T00:00:00Z');

// Находится ли date1 до date2?
console.log(date1 < date2); // true

// Является ли date1 тем же, что и date2?
console.log(date1.getTime() === date2.getTime()); // false

// Сколько дней между?
const diffTime = Math.abs(date2.getTime() - date1.getTime());
const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24)); 
console.log(diffDays); // 1
```

Пример вывода:

```
true
false
1
```

## Подробнее

Раньше даты представляли собой кучу форматов и запутанные расчеты. С JavaScript (и, соответственно, TypeScript) объект `Date` упростил вещи, стандартизировав то, как мы работаем со временем.

Альтернативы? Конечно. Библиотеки, такие как `moment.js` или `date-fns`, расширяют работу с датами дополнительной функциональностью. Но для базовых сравнений? Простота нативного Date зачастую справляется с задачей.

Под капотом `Date.getTime()` возвращает миллисекунды с эпохи (1 января 1970 года). Сравнение этих значений устраняет странности часовых поясов и високосных секунд, сводя всё к числам.

## См. также

- [Справочник по объекту Date от Mozilla Developer Network](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Date) для подробного изучения объектов Date.
- [Вам Не Нужен Moment.js](https://github.com/you-dont-need/You-Dont-Need-Momentjs) когда вам может быть, или не быть, нужна библиотека.
- [Официальная документация TypeScript](https://www.typescriptlang.org/docs/) для большего понимания сил и подводных камней TypeScript.
