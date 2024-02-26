---
date: 2024-01-20 17:46:33.121019-07:00
description: "\u0415\u043A\u0441\u0442\u0440\u0430\u043A\u0446\u0456\u044F \u043F\u0456\
  \u0434\u0440\u044F\u0434\u043A\u0456\u0432 \u0443 JavaScript \u2014 \u0446\u0435\
  \ \u043F\u0440\u043E\u0446\u0435\u0441 \u0432\u0438\u0431\u0456\u0440\u043A\u0438\
  \ \u0441\u043F\u0435\u0446\u0438\u0444\u0456\u0447\u043D\u0438\u0445 \u0447\u0430\
  \u0441\u0442\u0438\u043D \u0437 \u0440\u044F\u0434\u043A\u0430. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435, \u0449\u043E\u0431 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\
  \u044E\u0432\u0430\u0442\u0438 \u0442\u0435\u043A\u0441\u0442\u043E\u043C, \u0432\
  \u0430\u043B\u0456\u0434\u0443\u0432\u0430\u0442\u0438 \u0432\u0445\u0456\u0434\u043D\
  \u0456\u2026"
lastmod: '2024-02-25T18:49:47.383532-07:00'
model: gpt-4-1106-preview
summary: "\u0415\u043A\u0441\u0442\u0440\u0430\u043A\u0446\u0456\u044F \u043F\u0456\
  \u0434\u0440\u044F\u0434\u043A\u0456\u0432 \u0443 JavaScript \u2014 \u0446\u0435\
  \ \u043F\u0440\u043E\u0446\u0435\u0441 \u0432\u0438\u0431\u0456\u0440\u043A\u0438\
  \ \u0441\u043F\u0435\u0446\u0438\u0444\u0456\u0447\u043D\u0438\u0445 \u0447\u0430\
  \u0441\u0442\u0438\u043D \u0437 \u0440\u044F\u0434\u043A\u0430. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435, \u0449\u043E\u0431 \u043C\u0430\u043D\u0456\u043F\u0443\u043B\
  \u044E\u0432\u0430\u0442\u0438 \u0442\u0435\u043A\u0441\u0442\u043E\u043C, \u0432\
  \u0430\u043B\u0456\u0434\u0443\u0432\u0430\u0442\u0438 \u0432\u0445\u0456\u0434\u043D\
  \u0456\u2026"
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Екстракція підрядків у JavaScript — це процес вибірки специфічних частин з рядка. Програмісти роблять це, щоб маніпулювати текстом, валідувати вхідні дані, або просто для видобування інформації.

## How to: (Як Робити:)
```javascript
// Використання методу substr()
let text = "Привіт, мої друзі";
let slice = text.substr(7, 2); // "мо"

console.log(slice); // Виводить: мо

// Використання методу substring()
let subString = text.substring(7, 9);
console.log(subString); // Виводить: мо

// Використання методу slice()
let slicedText = text.slice(7, 9);
console.log(slicedText); // Виводить: мо
```

## Deep Dive (Поглиблений Розгляд)
Екстракція підрядків - стара як світ тема в програмуванні. JavaScript надає кілька методів: `substr()`, `substring()`, і `slice()`. Хоча `substr()` застарілий, його все ще можна зустріти у старому коді. `slice()` і `substring()` схожі, але `slice()` може приймати від'ємні індекси, які вказують позиції від кінця рядка. Обираючи метод, звертайте увагу на семантику та сумісність у вашому юз-кейсі.

## See Also (Дивіться Також)
- [MDN Web Docs: String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs: String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs: String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Stack Overflow: What is the difference between String.slice and String.substring?](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-string-slice-and-string-substring-in-javascript)
