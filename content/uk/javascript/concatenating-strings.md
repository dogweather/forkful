---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:23.126318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/concatenating-strings.md"
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
