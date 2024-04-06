---
date: 2024-01-20 17:46:33.121019-07:00
description: "How to: (\u042F\u043A \u0420\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-04-05T21:53:50.037078-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

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
