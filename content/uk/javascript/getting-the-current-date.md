---
title:                "Javascript: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Програмування - це цікаве та захоплююче заняття, яке дозволяє створювати різноманітні технології, програми та сайти. Один з найбільш універсальних і корисних навичок для програмістів - це робота з датами та часом. У цій статті ми розберемо, як за допомогою мови програмування Javascript отримати поточну дату та час.

## Як

Для початку, нам потрібно створити змінну, в яку будемо зберігати поточну дату. Давайте назвемо її "today". Ми можемо скористатися вбудованим об'єктом Date та методом "new Date()", щоб отримати поточну дату та час.

```Javascript
let today = new Date();
console.log(today);
```

В результаті ми отримаємо об'єкт Date, який містить поточну дату та час. Для того, щоб отримати більш зрозумілий вигляд дати, можна використовувати методи, які повертають конкретні частини дати, наприклад "getMonth()" або "getDay()".

```Javascript
let month = today.getMonth() + 1; // Додати 1, бо у Javascript місяці починаються з 0
let day = today.getDay();
let year = today.getFullYear();

console.log(`${day}.${month}.${year}.`);
```

Також, ми можемо використовувати різні формати дати та часу, змінюючи параметри методу "getDate()". Наприклад, для отримання часу в Гринвічі можна використовувати "getUTCDate()".

```Javascript
let hours = today.getUTCHours();
let minutes = today.getUTCMinutes();
let seconds = today.getUTCSeconds();

console.log(`${hours}:${minutes}:${seconds}`);
```

## Глибше

Крім методів отримання окремих частин дати, у об'єкта Date є також інші корисні методи, які дозволяють змінювати та встановлювати дату та час. Наприклад, "setDate()" дозволяє встановити новий день місяця, а "setFullYear()" - новий рік.

```Javascript
today.setFullYear(2021);
console.log(today);

// Output: Sun Nov 08 2021 20:16:03
```

Також, існує можливість створювати об'єкт Date за допомогою різних параметрів, наприклад, передавши рік, місяць та день.

```Javascript
let specificDate = new Date(2021, 0, 1); // рік, місяць (Починається з 0), день
console.log(specificDate);

// Output: Fri Jan 01 2021 00:00:00
```

Існує багато інших методів та можливостей, які дозволяють маніпулювати датами та часом у Javascript. Рекомендую ознайомитися з документацією, та е