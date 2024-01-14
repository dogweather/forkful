---
title:                "Javascript: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому?

Генерація випадкових чисел - це важлива техніка в програмуванні. Це допомагає створити різноманітність і необхідну випадковість в додатках і розробляти тести для різних значень.

## Як зробити?

```Javascript
// Генеруємо випадкове число від 1 до 10
let num = Math.floor(Math.random() * 10) + 1;
console.log(num); // приклад випадкового числа: 8

// Генеруємо випадкове число від 20 до 50
let num2 = Math.floor(Math.random() * (50 - 20 + 1)) + 20;
console.log(num2); // приклад випадкового числа: 25
```

## Глибоке дослідження

Математичний алгоритм, який використовується для генерації випадкових чисел, базується на певній формулі, яка використовується для отримання випадкових чисел в межах певного діапазону. Цей алгоритм також залежить від часу, тому кожного разу, коли ми генеруємо випадкове число, воно буде інше. Також оскільки це алгоритм, його можна повторно використовувати, щоб отримати більше випадкових чисел.

## Дивись також

- [JavaScript Math API](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Генерація випадкових чисел в JavaScript](https://www.informit.com/articles/article.aspx?p=615906&seqNum=9)