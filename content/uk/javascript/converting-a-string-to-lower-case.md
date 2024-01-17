---
title:                "Перетворення рядка у нижній регістр"
html_title:           "Javascript: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Що і чому?: Конвертування рядка в нижній регістр - це процес зміни всіх літер в рядку на їх нижній кейс. Програмісти часто роблять це для того, щоб зробити рядки однаковими для подальшої обробки та порівняння.

Як це зробити: 
```Javascript
let str = "HELLO WORLD!";
console.log(str.toLowerCase()); // виводить "hello world!"
```
У цьому прикладі ми використали вбудований метод `toLowerCase()`, який доступний для всіх рядків в Javascript. Цей метод повертає копію рядка, змінену на нижній кейс.

Озирнемося вглиб: 
Історичний контекст - у початкових версіях Javascript, методу `toLowerCase()` не було, тому програмісти використовували інші способи для зміни регістру (наприклад, змінювали ASCII коди символів). Однак, з появою ES5, метод `toLowerCase()` став стандартним і зручним способом.

Альтернативи - існують інші методи для зміни регістру, наприклад `toUpperCase()`, який змінює всі літери на верхній кейс. Але, якщо вам потрібно просто перевести рядок в нижній кейс, то `toLowerCase()` є найкращим варіантом.

Деталі реалізації - метод `toLowerCase()` використовує стандарт Unicode для переведення рядка в нижній регістр. Він також підтримує різні мови та складні символи.

Дивіться також: 
- [Метод toUpperCase ()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Стандарт Unicode](https://uk.wikipedia.org/wiki/%D0%A1%D1%82%D0%B0%D0%BD%D0%B4%D0%B0%D1%80%D1%82_Unicode)