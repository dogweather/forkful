---
title:                "З'єднання рядків"
html_title:           "Javascript: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і для чого?

З'єднання рядків є важливою функцією у багатьох програмах на Javascript. Воно дозволяє поєднувати різні фрагменти тексту для створення одного великого рядка. Це використовується для побудови повідомлень, конкатенації URL-адрес або створення складніших рядків для виведення на екран.

## Як це зробити?

Для з'єднання рядків у Javascript використовується спеціальний оператор ```+```. Ось приклад коду:

```javascript
let firstName = "Олег";
let lastName = "Шевчук";

console.log(firstName + " " + lastName); // Виведе: Олег Шевчук
```

У цьому прикладі ми використовуємо оператор ```+``` для з'єднання двох рядків - значення змінних `firstName` і `lastName` - разом з пробілом між ними.

## Глибока підготовка

Історично оператор ```+``` використовувався тільки для обчислення арифметичних виразів, але з появою Javascript став доступний також для з'єднання рядків. Існують також інші способи з'єднання рядків, наприклад, метод `.concat()`, але оператор ```+``` є більш коротким та полегшує читання коду.

Щоб уникнути помилок при з'єднанні чисел та рядків, рекомендується використовувати метод `.toString()` для перетворення чисел у рядкового типу даних перед їх з'єднанням.

## Дивись також

[MDN - З'єднання рядків у Javascript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/concat)

[MDN - Оператор з'єднання (+)](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Operators/Addition)