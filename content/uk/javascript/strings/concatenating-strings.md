---
date: 2024-01-20 17:35:23.126318-07:00
description: "\u041A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0456\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\
  \u0456\u0431 \u0437'\u0454\u0434\u043D\u0430\u0442\u0438 \u0434\u0432\u0430 \u0430\
  \u0431\u043E \u0431\u0456\u043B\u044C\u0448\u0435 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0432 \u043E\u0434\u0438\u043D. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  , \u0449\u043E\u0431 \u0441\u0442\u0432\u043E\u0440\u044E\u0432\u0430\u0442\u0438\
  \ \u0434\u0438\u043D\u0430\u043C\u0456\u0447\u043D\u0456 \u0442\u0435\u043A\u0441\
  \u0442\u0438, \u0437\u043C\u0456\u043D\u043D\u0456 \u043F\u043E\u0432\u0456\u0434\
  \u043E\u043C\u043B\u0435\u043D\u043D\u044F, \u0430\u0431\u043E\u2026"
lastmod: '2024-03-13T22:44:49.979542-06:00'
model: gpt-4-1106-preview
summary: "\u041A\u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0456\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\
  \u0456\u0431 \u0437'\u0454\u0434\u043D\u0430\u0442\u0438 \u0434\u0432\u0430 \u0430\
  \u0431\u043E \u0431\u0456\u043B\u044C\u0448\u0435 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0432 \u043E\u0434\u0438\u043D. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  , \u0449\u043E\u0431 \u0441\u0442\u0432\u043E\u0440\u044E\u0432\u0430\u0442\u0438\
  \ \u0434\u0438\u043D\u0430\u043C\u0456\u0447\u043D\u0456 \u0442\u0435\u043A\u0441\
  \u0442\u0438, \u0437\u043C\u0456\u043D\u043D\u0456 \u043F\u043E\u0432\u0456\u0434\
  \u043E\u043C\u043B\u0435\u043D\u043D\u044F, \u0430\u0431\u043E\u2026"
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
---

{{< edit_this_page >}}

## What & Why? / Що таке конкатенація рядків і навіщо вона потрібна?

Конкатенація рядків — це спосіб з'єднати два або більше рядків в один. Програмісти роблять це, щоб створювати динамічні тексти, змінні повідомлення, або просто об'єднувати слова та фрази.

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
