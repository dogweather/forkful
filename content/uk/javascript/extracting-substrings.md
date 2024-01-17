---
title:                "Видобування підрядків"
html_title:           "Javascript: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що то таке і чому це робиться?
Вилучення підрядків - це процес отримання певної частини рядка з використанням визначених позицій або індексів. Це корисний інструмент для програмістів, оскільки дозволяє працювати з певними частинами тексту, а не з усім рядком. Наприклад, ви можете використовувати вилучення підрядків, щоб отримати лише ім'я користувача з повного електронного листа, ігноруючи решту тексту.

## Як це зробити:
```Javascript
// Використання методу substr()
let fullText = "Привіт, Михайло!";
let name = fullText.substr(7, 7); // Починаємо зі зсуву позицій 7 та вилучаємо наступні 7 символів
console.log(name); // Михайло

// Використання методу slice()
let message = "Вчора був дуже гарний день.";
let part = message.slice(17); // Використовуємо позицію, з якої потрібно почати вилучення
console.log(part); // гарний день.
```

## Крок у глиб:
Вилучення підрядків використовується в програмуванні вже багато років і є частиною багатьох мов програмування. Наприклад, у мові Java використовуються методи substring() та subSequence(), а у мові Python - методи slice(). Є також альтернативи, такі як регулярні вирази, але вилучення підрядків є простішим інструментом для роботи з текстом.

## Дивіться також:
- [Документація з методом substr() в MDN](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [Приклади використання методу slice() на w3schools](https://www.w3schools.com/jsref/jsref_slice_string.asp)