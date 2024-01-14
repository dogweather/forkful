---
title:                "TypeScript: Отримання поточної дати"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Отримання поточної дати є важливою частиною багатьох програм, які ми використовуємо щодня. Незалежно від того, чи це веб-додаток, мобільний додаток або навіть годинник на нашому смартфоні, ми часто потребуємо знати поточний час і дату. У цьому блозі ми розглянемо, як отримати поточну дату в TypeScript та чому це корисно знати.

## Як

Отримання поточної дати в TypeScript є простим завданням за допомогою вбудованого об'єкта Date. Ось приклад коду, який допоможе вам отримати поточну дату у форматі день/місяць/рік:

```TypeScript
let currentDate = new Date();
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1;
let year = currentDate.getFullYear();
console.log(`${day}/${month}/${year}`); 
```

Цей код створює новий об'єкт Date з поточною датою і потім отримує окремі значення для дня, місяця та року. Наступним кроком є використання цих значень для форматування поточної дати у зручному для вас форматі.

## Глибинний аналіз

Об'єкт Date в TypeScript має багато інших корисних методів та властивостей, які можуть бути корисними при роботі з датами. Наприклад, метод `getDay()` повертає номер дня тижня (від 0 до 6), а `getTime()` повертате кількість мілісекунд, які пройшли з 1 січня 1970 року. Ви можете детальніше дослідити це об'єкт і його можливості в [офіційній документації](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date).

## Дивіться також

- [JavaScript Date об'єкт](https://www.w3schools.com/js/js_dates.asp)
- [Використання дат в TypeScript](https://www.techiediaries.com/date-timestamp-todays-date-typescript-javascript/)
- [Обробка дати в TypeScript](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)