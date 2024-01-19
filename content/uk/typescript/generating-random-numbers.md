---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Генерація випадкових чисел - це процес створення числових значень, які не можуть бути передбачені точніше, ніж випадковим вибором. Програмісти це роблять, щоб внести випадковість або непередбачуваність в програми та алгоритми.

## Як це робити?

Використовуйте вбудовану функцію `Math.random()`. Вона повертає випадкове десяткове число від 0 (включно) до 1 (не включно).

```TypeScript
const random = Math.random();
console.log(random);
```

Перетворимо це на випадкове ціле число від 1 до 10.

```TypeScript
const randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt);
```

## Поглиблений огляд

(1) Історичний контекст: комп'ютерні генератори випадкових чисел були описані ще в 1946 році відомим математиком Джоном фон Нейманом. 

(2) Альтернативи: помімо `Math.random()`, в JavaScript/TypeScript існують інші методи для генерації випадкових чисел, наприклад, бібліотека `crypto.getRandomValues()`.

(3) Деталі реалізації: JavaScript (і, отже, TypeScript) використовують генератор випадкових чисел XorShift128+. Він створює числа з дуже високою статистичною якістю і безпечний для криптографічного використання.

## Зверніть увагу також на

* [MDN Web Docs: Math.random()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)

* [MDN Web Docs: crypto.getRandomValues()](https://developer.mozilla.org/uk/docs/Web/API/Crypto/getRandomValues) 

* [StackOverflow: Генерація випадкових чисел в TypeScript](https://stackoverflow.com/questions/1527803/generating-random-whole-numbers-in-javascript-in-a-specific-range)