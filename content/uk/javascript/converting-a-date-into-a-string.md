---
title:                "Javascript: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Чому

Програмістам часто доводиться працювати з датами в своїх проектах. Одним із часто зустрічаються завдань є конвертація дати в рядок. В цій статті я хочу розповісти, чому це корисно та як це зробити за допомогою Javascript.

## Як це зробити

Для початку, нам потрібно створити об'єкт дати за допомогою конструктора Date(). Наприклад, щоб отримати поточну дату, можна використати наступний код:

```Javascript
let date = new Date();
```

Далі, для конвертації дати в рядок, нам потрібно використати метод toDateString(). Цей метод повертає дату у вигляді рядка в форматі "MMM DD YYYY" (де MMM - скорочення назви місяця, DD - день місяця, YYYY - рік).

Наприклад, якщо ми хочемо отримати рядок з поточною датою, наш код буде виглядати так:

```Javascript
let date = new Date();
let dateString = date.toDateString();
console.log(dateString);

// Output: "Jul 11 2021"
```

Ми також можемо вказати бажаний формат дати, використовуючи метод toLocaleDateString(). Цей метод приймає два параметри - мову та об'єкт з опціями форматування.

Наприклад, якщо ми хочемо отримати рядок з поточною датою в форматі "день.місяць.рік", можна використати наступний код:

```Javascript
let date = new Date();
let options = { day: "numeric", month: "long", year: "numeric" };
let dateString = date.toLocaleDateString("uk-UA", options);
console.log(dateString);

// Output: "11 липня 2021 р."
```

## Глибші роздуми

Під капотом, Javascript використовує метод toString() для конвертації дати в рядок. Цей метод повертає дату у вигляді рядка в форматі "Day Month Date Year Hours:Minutes:Seconds Timezone Offset (GMT)".

Також варто зазначити, що метод toDateString() не підтримує стандартизованого формату, тому рядок дати може відрізнятись залежно від браузера.

## Дивись також

- [MDN - Date.prototype.toDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [MDN - Date.prototype.toLocaleDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [MDN - Date.prototype.toString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)