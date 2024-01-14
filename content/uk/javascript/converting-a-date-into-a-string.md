---
title:    "Javascript: Перетворення дати в рядок"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому
Заміна дати в рядок це необхідна частина програмування, особливо коли має значення як дата буде відображена для користувача.

## Як це зробити
```Javascript
// Створення нового об'єкта дати з поточною датою
const date = new Date();

// Використання методу toLocaleString() для конвертації в рядок
const dateString = date.toLocaleString();

// Виведення результату в консоль
console.log(dateString); // "10/12/2021, 11:08:34 AM"
```

## Глибокий занурення
При конвертації дати в рядок, важливо враховувати часовий пояс та формат виведення. Метод toLocaleString() приймає два параметри для вказівки бажаного мови та формату. Також можна використовувати метод toDateString() для виведення дати без часу. Для точного визначення формату можна використовувати інші методи, такі як toLocaleDateString() та toLocaleTimeString(). Якщо потрібно додати додаткові параметри, можна використати бібліотеки, такі як Moment.js.

## Дивись також
- [Метод toLocaleString() в документації MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Бібліотека Moment.js](https://momentjs.com/)
- [Використання міжнародних дат та часових поясів в рядках в JavaScript](https://stackoverflow.com/questions/16662660/internationalization-with-dates-and-time-zones-in-javascript)