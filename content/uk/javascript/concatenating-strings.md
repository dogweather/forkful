---
title:                "Javascript: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

При програмуванні на Javascript необхідно часто з'єднувати різні рядки, щоб створити більш складні дані. Це може бути корисно, наприклад, при створенні повідомлень для користувачів або URLs для передачі даних.

## Як це зробити

```Javascript
// Створюємо два рядки
let firstName = "Василь";
let lastName = "Петренко";

// З'єднуємо їх за допомогою "+" оператора
let fullName = firstName + " " + lastName;

// Виводимо результат у консоль
console.log(fullName); // Василь Петренко
```

```Javascript
// Також можна використовувати метод concat()
let age = 25;
let message = "Я " + fullName.concat(", мені ") + age + " років.";

// Виводимо повідомлення у консоль
console.log(message); // Я Василь Петренко, мені 25 років.
```

## Глибоке погруження

При з'єднанні рядків, Javascript перетворює всі дані в рядковий тип даних. Тому, якщо потрібно з'єднати числа та рядки, необхідно перетворити числа на рядки за допомогою методу toString().

Наприклад:
```Javascript
// Створюємо змінну з числовим значенням
let num = 10;

// Перетворюємо на рядковий тип даних і з'єднуємо з рядком
let result = "Загальна кількість: " + num.toString();

// Результат: "Загальна кількість: 10"
```

## Ви можете також поцікавитись

- [Javascript string concatenation](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [MDN Docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Типи даних у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Data_structures)