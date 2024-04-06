---
date: 2024-01-20 17:35:23.126318-07:00
description: "How to / \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: ."
lastmod: '2024-04-05T21:53:50.040693-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

## How to / Як це зробити:
```javascript
// Із застосуванням оператора +
let greeting = "Привіт, " + "світ!";
console.log(greeting); // Вивід: Привіт, світ!

// Із застосуванням шаблонних літералів (template literals)
let userName = "Олексій";
let personalizedGreeting = `Вітаю, ${userName}!`;
console.log(personalizedGreeting); // Вивід: Вітаю, Олексій!
```

## Deep Dive / Поглиблений огляд:
Довгий час в JavaScript для конкатенації рядків використовували лише оператор `+`. Після ES6 (ECMAScript 2015) з'явилися шаблонні літерали, які спростили вставку змінних в рядки за допомогою позначення `${}`. Щодо виконання, процесори JS оптимізують конкатенацію, але шаблонні літерали можуть бути швидшими за складніші операції завдяки меншій кількості тимчасових рядків.

Альтернативи: метод `concat()` рядків (який рідко використовується через його громіздкість), архаїчний метод `join()` масивів (добре налагоджений, але дещо вдалий від теми). Програмісти часто обирають шаблонні літерали для читабельності та легкості внесення змін.

## See Also / Дивіться також:
- MDN про конкатенацію рядків: [Concatenating strings](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- Шаблонні літерали на MDN: [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [ECMAScript 2015 (ES6) specification](https://www.ecma-international.org/ecma-262/6.0/)
