---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Конвертування дати в рядок - це процес, при якому береться об'єкт дати та перетворюється в інший формат, зокрема в рядок. Програмісти роблять це, щоб полегшити відображення, зберігання або передачу інформації про дату.

## Як це зробити:

У JavaScript для конвертування дати в рядок ми використовуємо метод `.toString()`. Ось приклад:

```Javascript
let today = new Date();
console.log(today.toString());
```

Виходьте виконає виведення поточної дати і часу як рядка, наприклад: 
`Wed Jul 28 2021 12:10:33 GMT+0200 (Eastern European Standard Time)`

## Більш глибоке занурення

(1) Історичний контекст: Конвертування дат у рядки існує з перших днів програмування, адже рядки - це гнучкий та універсальний спосіб представлення данних.

(2) Альтернативи: JavaScript також надає методи, як `.toISOString()`, щоб отримати рядок в форматі ISO, або `.toDateString()`, щоб отримати рядок дати без часу.

(3) Деталі реалізації: Внутрішньо, метод `.toString()` конвертує об'єкт Date в рядок, використовуючи стандартний формат дати JavaScript.

## Дивитись також

1. [JavaScript Date toString() Method](https://www.w3schools.com/jsref/jsref_tostring_date.asp) - детальніше про метод `.toString()` в JavaScript.
2. [JavaScript Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) - докладний посібник по датам в JavaScript з документації Mozilla.
3. [An Overview of JavaScript Dates](https://www.digitalocean.com/community/tutorials/understanding-date-and-time-in-javascript) - гарний огляд роботи з датами в JavaScript від DigitalOcean.