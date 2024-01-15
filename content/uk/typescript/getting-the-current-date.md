---
title:                "Отримання поточної дати"
html_title:           "TypeScript: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому
Існує багато ситуацій, коли вам потрібно обробити поточну дату у вашому коді, наприклад, створити логи або записати дату створення елементу. TypeScript надає нам потужні засоби для отримання теперішньої дати без додаткових зусиль.

## Як
Щоб отримати поточну дату в TypeScript, нам потрібно використовувати вбудований об'єкт `Date`. У нього є кілька методів та властивостей, за допомогою яких ми можемо отримати інформацію про поточну дату. Давайте подивимося на деякі приклади коду:

```TypeScript
// Отримання поточної дати
const now = new Date();

// Отримання поточної дати у вигляді рядка
const dateString = now.toString();

// Отримання поточного часу у годинах, хвилинах та секундах
const hours = now.getHours();
const minutes = now.getMinutes();
const seconds = now.getSeconds();
```

В прикладі вище ми створили об'єкт `Date`, який представляє поточну дату та час. Ми також використали метод `toString()`, щоб отримати поточну дату у вигляді рядка. Крім того, ми використали методи `getHours()`, `getMinutes()` та `getSeconds()`, щоб отримати поточний час у годинах, хвилинах та секундах.

## Глибоке занурення
Об'єкт `Date` у TypeScript базується на стандартному об'єкті `Date` у JavaScript, і має деякі зауваження стосовно часових зон та точності. Якщо вам потрібна більш детальна інформація, ви можете переглянути документацію на [офіційному сайті TypeScript](https://www.typescriptlang.org/docs/handbook/utility-types.html#picktype-keys).

## Дивіться також
- [Робота з датами та часом в TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-typescript)
- [Стандартний об'єкт Date у JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Official TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/utility-types.html)