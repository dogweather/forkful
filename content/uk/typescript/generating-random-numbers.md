---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:50:06.905133-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Генерація випадкових чисел – це процес, який випльовує числа, які немає як передбачити. У програмуванні випадкові числа корисні для всього – від простого вибору елемента до високих ступенів захисту.

## Як це зробити:

Для створення випадкового числа у TypeScript, використовуйте `Math.random()`. Ось приклад:

```TypeScript
function getRandomNumber(min: number, max: number): number {
  // Вираховуємо випадкове число між min (включно) та max (виключно)
  return Math.random() * (max - min) + min;
}

// Отримуємо випадкове число між 1 та 10
console.log(getRandomNumber(1, 10));
```

**Sample Output:**
```
5.301943394183662
```

## Глибше занурення

Перший механізм генерації випадкових чисел було створено у стародавні часи – використовували кістки та жеребки. В епоху комп'ютерів створили алгоритми, такі як лінійний конгруентний метод. Зауважте, що `Math.random()` у JavaScript та TypeScript не є повністю випадковим; це псевдовипадковий, що значить результат генерується алгоритмічно.

Альтернативою є криптографічно стійкі генератори випадкових чисел (`Crypto.getRandomValues()` для браузерів), які забезпечують більшу непередбачуваність для захисту даних.

Говорячи про те, як працює `Math.random()`, вона використовує різні алгоритми в залежності від середовища виконання, але найчастіше це Xorshift або його варіації.

## Дивіться також:

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs: Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [Wikipedia: Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)