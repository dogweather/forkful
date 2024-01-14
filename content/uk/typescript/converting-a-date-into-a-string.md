---
title:                "TypeScript: Перетворення дати у рядок"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

В першу чергу, конвертація дати в рядок є важливим процесом для програмістів, що працюють з TypeScript. Рядок дати може бути необхідним для подальших операцій в програмі, таких як збереження, виведення на екран, передача на сервер тощо.

## Як це зробити

Для початку, ми повинні створити змінну з типом Date, яка буде містити поточну дату і час:

```TypeScript
const currentDate: Date = new Date();
```

Далі, ми можемо використовувати методи Date для отримання окремих частин дати, таких як день, місяць та рік:

```TypeScript
const day: number = currentDate.getDate();
const month: number = currentDate.getMonth();
const year: number = currentDate.getFullYear();
```

Для перетворення цих чисел у рядок, ми можемо використовувати функцію `toString()`:

```TypeScript
const dateString: string = day.toString() + "/" + month.toString() + "/" + year.toString();
```

У кінці, ми отримаємо рядок, який містить дату у форматі "день/місяць/рік".

Нижче наведений вихід програми для поточної дати:

```TypeScript
console.log(dateString);
// 15/3/2021
```

## Глибше дослідження

Окрім методів, що використовуються в прикладі вище, TypeScript має ще декілька корисних функцій для роботи з датами.

Наприклад, метод `toLocaleDateString()` дозволяє виводити дату у вигляді, придатному для користувача, враховуючи регіональні налаштування:

```TypeScript
const options: Intl.DateTimeFormatOptions = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const localeDateString: string = currentDate.toLocaleDateString('uk-UK', options);

console.log(localeDateString);
// понеділок, 15 березня 2021 р.
```

Крім того, TypeScript має вбудований тип `Date`, який містить потужні методи для роботи з датою і часом. Детальніше про цей тип можна дізнатися у [документації](https://www.typescriptlang.org/docs/handbook/utility-types.html#date).

## Дивіться також

- [Керування датою та часом в TypeScript](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Робота з рядками в TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Методи роботи з датою в TypeScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)