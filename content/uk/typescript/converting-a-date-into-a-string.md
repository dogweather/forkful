---
title:    "TypeScript: Перетворення дати в рядок"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Чому

Конвертація дати в рядок є важливою частиною розробки програмного забезпечення, оскільки дати часто використовуються для збереження та виведення інформації. В цій статті ми дізнаємося, як конвертувати дату в рядок за допомогою TypeScript.

## Як

Для початку, створимо змінну з датою, яку ми хочемо конвертувати:

```TypeScript
let date = new Date();
```

Тепер використаємо метод `toLocaleDateString()` для конвертації дати в рядок:

```TypeScript
let stringDate = date.toLocaleDateString();
```

Тепер, якщо ми виведемо змінну `stringDate`, ми побачимо, що дата була конвертована в рядок:

```TypeScript
console.log(stringDate); // Виведе "29.08.2021"
```

Метод `toLocaleDateString()` також має параметр для визначення формату дати:

```TypeScript
let stringDate = date.toLocaleDateString('uk-UA', {year: 'numeric', month: 'long', day: 'numeric'});
```

Цей код виведе дату у форматі "29 серпня 2021 року".

## Глибоке дослідження

Крім методу `toLocaleDateString()`, TypeScript також має інші вбудовані методи для конвертації дати в рядок. Наприклад, метод `toString()` повертає дату у форматі `"Sun Aug 29 2021 00:00:00 GMT+0300 (Eastern European Summer Time)"`.

Також можна використовувати сторонні бібліотеки, які мають більше можливостей для форматування дати. Наприклад, бібліотека Moment.js дозволяє конвертувати дату в різноманітні формати та здійснювати розрахунки з датами.

## Дивіться також

- [Документація TypeScript з методами дати](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Метод `toLocaleDateString()`](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Огляд бібліотеки Moment.js](https://momentjs.com/docs/#/displaying/)