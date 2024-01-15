---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Javascript: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

В даний час програмування є необхідною умінням, яка може допомогти у розв'язанні багатьох проблем. Розрахунок дати в майбутньому або минулому є необхідною задачею для багатьох проектів, яка може бути вирішена за допомогою Javascript.

## Як

Одним з шляхів розрахунку дати в майбутньому або минулому є використання вбудованих функцій ```Date()``` та ```setFullYear()```. Нижче показаний приклад коду для обчислення дати за допомогою цих функцій:

```Javascript
// обчислення дати за 10 днів вперед
var now = new Date();
now.setDate(now.getDate() + 10);
console.log(now); // виведе дату через 10 днів

// обчислення дати за 3 роки назад
var now = new Date();
now.setFullYear(now.getFullYear() - 3);
console.log(now); // виведе дату 3 роки тому
```

## Deep Dive

Основна концепція щодо розрахунку дати в майбутньому або минулому полягає в тому, щоб використовувати дату за замовчуванням і додавати (або віднімати) необхідну кількість днів чи років за допомогою вбудованих функцій.

Важливо також звернути увагу на формат виведення дати, який може бути змінений за допомогою функції ```toLocaleDateString()``` та передання потрібних параметрів. Докладніше про це можна дізнатися у [документації](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).

## Дивіться також

- [W3Schools: Робота з датою і часом в Javascript](https://www.w3schools.com/js/js_dates.asp)
- [MDN: Робота з датою в Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Codecademy: Онлайн курс з Javascript](https://www.codecademy.com/learn/introduction-to-javascript)