---
title:    "Javascript: Знаходження довжини рядка"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні іноді необхідно отримати довжину рядка, наприклад, для валидації даних або для перевірки коректності введення користувачем. Знання, як знайти довжину рядка в Javascript, є важливим навичкою для будь-якого програміста.

## Як

Щоб знайти довжину рядка в Javascript, використовуйте метод `length`. Наприклад:

```Javascript
let str = "Привіт, друзі!";
console.log(str.length); // виведе 14
```

Також `length` можна використовувати для масивів:

```Javascript
let arr = ["яблуко", "банан", "апельсин"];
console.log(arr.length); // виведе 3
```

## Глибокий аналіз

Метод `length` повертає кількість елементів в рядку або масиві. Він доступний для будь-якого рядка чи масиву в Javascript. Цей метод є невластивим для чисел та буде повертати `undefined`.

Існують також деякі інтересні використання методу `length` в Javascript. Наприклад, ви можете використовувати його для перевірки, чи є рядок пустим:

```Javascript
let str = "";
if (str.length === 0) {
  console.log("Рядок пустий");
}
```

Або для отримання останнього елементу з масиву:

```Javascript
let arr = ["яблуко", "банан", "апельсин"];
let lastElement = arr[arr.length - 1]; // отримаємо "апельсин"
```

## Дивись також

- [MDN документація про метод `length`](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools - довжина рядка в Javascript](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [Стаття на іншій мові - як знайти довжину рядка в Javascript](https://www.toptal.com/javascript/javascript-string-length-property-cheatsheet)