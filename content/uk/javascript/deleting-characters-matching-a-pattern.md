---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:43:01.967809-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Що та чому?)
Видалення символів за шаблоном дозволяє нам очистити строки від непотрібних елементів. Це важливо для валідації вхідних даних, обробки тексту чи просто щоб упорядкувати дані.

## How to: (Як це зробити:)
```Javascript
// Приклад: видалення нечисельних символів із строки
let phone = 'Phone: +38(050)123-45-67';
let digitsOnly = phone.replace(/\D/g, ''); // Використовуємо регулярний вираз
console.log(digitsOnly); // Виводить "380501234567"

// Приклад: видалення пробільних символів з початку та кінця строки
let greeting = '  Привіт, світ!  ';
let trimmed = greeting.trim(); // Використовуємо метод trim()
console.log(trimmed); // Виводить "Привіт, світ!"
```

## Deep Dive (Занурення вглибину):
Видалення символів за шаблоном використовується давно як один із фундаментальних інструментів обробки текстів. Історично, це було частиною багатьох мов програмування, адже воно корисне в багатьох задачах, як от очищення даних чи аналіз текстів.

У JavaScript для таких цілей найчастіше використовують регулярні вирази та метод `replace()`. Альтернативою може бути використання методів `slice()`, `substring()`, чи `substr()`, але ці методи менш гнучкі для шаблонного видалення.

Що до самих регулярних виразів, символ `\D` відповідає будь-якому нечисельному символу, в той час як глобальний прапорець `g` забезпечує пошук по всій строці, а не тільки перше співпадіння. Метод `trim()` з'явився в стандарті ECMAScript 5 і є зручним для видалення пробільних символів на початку та в кінці стрічки.

## See Also (Дивіться також):
- [MDN Web Docs: RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: String.prototype.trim()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/Trim)
