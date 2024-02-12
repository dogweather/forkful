---
title:                "Преобразование даты в строку"
aliases: - /ru/typescript/converting-a-date-into-a-string.md
date:                  2024-01-28T23:56:36.892851-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование даты в строку"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Преобразование даты в строку меняет объект даты на текстовый формат. Программисты делают это для удобочитаемости, хранения или для отображения дат пользователем.

## Как сделать:

```TypeScript
// Простое преобразование с использованием toLocaleString()
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // "4/3/2023, 1:15:30 PM" (будет варьироваться в зависимости от локали)

// Формат ISO с использованием toISOString()
let isoString = date.toISOString();
console.log(isoString); // "2023-04-03T13:15:30.000Z"

// Пользовательский формат с использованием toLocaleDateString()
let customString = date.toLocaleDateString('en-US', {
  year: 'numeric',
  month: 'long',
  day: 'numeric',
});
console.log(customString); // "Апрель 3, 2023"
```

## Подробнее

Подумайте о строковом формате даты как о её паспорте, позволяющем ей пересекать системные границы - от баз данных до веб-страниц. Исторически мы сталкивались с несоответствием форматов дат, поэтому были введены стандарты, такие как ISO 8601. Это упрощает обмен датами по всему миру.

Альтернативы встроенным методам? Библиотеки! Moment.js был основным выбором на протяжении многих лет, но в наши дни предпочтение отдаётся date-fns или Luxon — они легче и более модульные.

Суть этих преобразований лежит в используемых методах. `toLocaleString()` опирается на локаль пользователя, что делает его идеальным для отображения пользователю. `toISOString()`, однако, остаётся верным формату ISO 8601, что блестяще подходит для сериализации и хранения дат в стандартном формате. А `toLocaleDateString()` дает вам контроль над внешним видом, удовлетворяя специфическим потребностям в стилизации.

## Смотрите также

- [Объект Date - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [документация date-fns](https://date-fns.org/docs/Getting-Started)
- [документация Luxon](https://moment.github.io/luxon/)
- [Формат даты и времени по ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
