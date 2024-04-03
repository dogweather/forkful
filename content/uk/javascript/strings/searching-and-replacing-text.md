---
changelog:
- 2024-03-13, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-13 16:16:02.436980-06:00
description: "\u041F\u043E\u0448\u0443\u043A \u0456 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u043E\u0437\u043D\u0430\u0447\u0430\
  \u0454 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043F\
  \u0435\u0432\u043D\u0438\u0445 \u043F\u0456\u0434\u0440\u044F\u0434\u043A\u0456\u0432\
  \ \u0456 \u0457\u0445\u043D\u044E \u0437\u0430\u043C\u0456\u043D\u0443 \u043D\u0430\
  \ \u0449\u043E\u0441\u044C \u043D\u043E\u0432\u0435. \u041D\u0430\u0432\u0456\u0449\
  \u043E \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438? \u0426\u0435 \u0441\u043A\
  \u0440\u0456\u0437\u044C: \u0432\u0438\u043F\u0440\u0430\u0432\u043B\u0435\u043D\
  \u043D\u044F \u043F\u043E\u043C\u0438\u043B\u043E\u043A \u0443 \u0434\u043E\u043A\
  \u0443\u043C\u0435\u043D\u0442\u0456,\u2026"
lastmod: '2024-03-13T22:44:49.966802-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0456 \u0437\u0430\u043C\u0456\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0443 \u043E\u0437\u043D\u0430\u0447\u0430\u0454\
  \ \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043F\u0435\
  \u0432\u043D\u0438\u0445 \u043F\u0456\u0434\u0440\u044F\u0434\u043A\u0456\u0432\
  \ \u0456 \u0457\u0445\u043D\u044E \u0437\u0430\u043C\u0456\u043D\u0443 \u043D\u0430\
  \ \u0449\u043E\u0441\u044C \u043D\u043E\u0432\u0435."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## Як це зробити:
В JavaScript для цього є `String.prototype.replace()`. Передайте рядок або регулярний вираз і заміну. Ось швидкий і нехитрий приклад:

```javascript
let str = "I love to code in JavaScript!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // Виводить: I love to code in TypeScript!
```

Тепер, з регулярним виразом для глобальних замін:

```javascript
let story = "The quick brown fox jumps over the lazy dog. The fox is clever.";
let newStory = story.replace(/fox/g, "cat");
console.log(newStory); // Виводить: The quick brown cat jumps over the lazy dog. The cat is clever.
```

## Поглиблено
Озираючись назад, `String.prototype.replace()` є в JS з самого початку — часів Netscape 2. А тепер, ES6 приніс нам шаблонні літерали та стрілкові функції, які ще більше полегшили роботу з регулярними виразами завдяки більш стислому та зрозумілому коду.

Альтернативи? Звичайно. Якщо ви працюєте з обробкою тексту великого масштабу, ви можете перейти на потоки Node.js або використовувати зовнішні бібліотеки для обробки складних патернів, ефективності та продуктивності.

Що стосується впровадження, сама `replace()` є простою. Але регулярні вирази можуть стати складними. Починайте з простого, вивчіть спеціальні символи (`.` відповідає будь-якому символу, `*` для повторюваних патернів) та тестуйте за допомогою таких інструментів як regex101.

## Дивіться також
- Документація MDN про replace: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 для тестування виразів: https://regex101.com/
- Інформація про регулярні вирази в JavaScript: https://javascript.info/regexp-introduction
