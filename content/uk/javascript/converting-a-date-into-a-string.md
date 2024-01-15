---
title:                "Перетворення дати в рядок"
html_title:           "Javascript: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому
JavaScript є однією з найбільш популярних мов програмування, тому це стає необхідністю для майже кожного програміста знати його. Конвертування дати в рядок є важливою навичкою, яка може знадобитися при розробці веб-додатків або плагінів.

## Як це зробити
```javascript
// Створимо змінну зі значенням дати
let myDate = new Date("December 25, 2021");

// Застосуємо метод .toString() для конвертації дати в рядок
let myString = myDate.toString();

// Виведемо результат у консоль
console.log(myString); // "Sat Dec 25 2021 00:00:00 GMT+0200 (Eastern European Standard Time)"
```

Також, є можливість використання різних опцій для змінення формату виведеної дати. Наприклад:

```javascript
// Використовуйте опцію 'short' для скороченого формату
let myString = myDate.toLocaleDateString('uk-UA', {dateStyle: 'short'});

// Результат: 25.12.21
```

## Глибоке дослідження
Якщо ви бажаєте глибше розібратися з процесом конвертації дати в рядок, вам необхідно розібратися з об'єктом Date в JavaScript. Цей об'єкт містить методи, які дозволяють отримувати окремі частини дати та зберігати їх у вигляді числових значень.

Наприклад, метод .getDate() повертає число, яке представляє день місяця у даті. Існують також методи для отримання місяця, року, годин, хвилин та секунд у даті.

## Дивіться також
- [Об'єкт Date в документації JavaScript] (https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Робота з датами в JavaScript] (https://www.w3schools.com/js/js_dates.asp)
- [Конвертація дати в рядок в JavaScript] (https://www.tutorialspoint.com/How-to-convert-Unix-Timestamp-formatted-date-to-Human-Readable-in-JavaScript)

**PowerNap**: В старих браузерах (наприклад, Internet Explorer 8) можуть виникнути проблеми з підтримкою деяких опцій конвертації дати в рядок. Наприклад, опція 'short' може виводити неправильний формат. Тому рекомендується завжди перевіряти сумісність вашого коду з різними браузерами перед публікацією.