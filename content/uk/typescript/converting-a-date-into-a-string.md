---
title:                "Перетворення дати в рядок"
html_title:           "TypeScript: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

У простій розмові ми часто використовуємо дату у форматі строки, наприклад, "4 жовтня 2021". Науковці бали б називати це художньою свободою, але для програмістів це справжня проблема. Тому ми вивчимо, як перетворити дату в рядок за допомогою TypeScript для ефективнішої роботи з даними.

## Як

Конвертація дати в рядок може здатися складною, але TypeScript дає нам потужні інструменти, щоб виконати це легко. Ось приклад коду з використанням вбудованої функції `toString()` та форматування з допомогою бібліотеки moment.js:

```TypeScript
let currentDate = new Date();
let dateString = currentDate.toString();
console.log(dateString); // Mon Oct 04 2021 00:00:00 GMT-0700 (Pacific Daylight Time)

let formattedDate = moment(currentDate).format('MMMM Do YYYY');
console.log(formattedDate); // October 4th 2021
```

## Глибокий погляд
Все починається з об'єкта Date, який містить всю інформацію щодо часу та дати. Для конвертації у рядок ми можемо використовувати метод `toString()`, який повертає стандартне представлення дати, але це не завжди зручно. Тому часто програмісти вибирають використання бібліотек для форматування дати у зручний для них спосіб.

## Дивись також
- [Офіційна документація TypeScript](https://www.typescriptlang.org/docs/)
- [Бібліотека moment.js](https://momentjs.com/)
- [Розширена конвертація дати в рядок в TypeScript](https://spin.atomicobject.com/2020/01/17/format-date-string-typescript/)