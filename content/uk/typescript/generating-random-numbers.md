---
title:                "TypeScript: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

У галузі програмування на TypeScript, генерування випадкових чисел є важливою та корисною функцією. Вона дозволяє створювати унікальні та випадкові дані для тестування, створення ігор та інших застосувань. Також, вона може бути використана для реалізації багатьох інших алгоритмів.

## Як

Для початку, нам необхідно імпортувати вбудований модуль `Math`, який дозволить нам використовувати функції для роботи з числами. Далі, ми можемо просто використовувати функцію `random()` для створення випадкових чисел від 0 до 1.

```TypeScript
import { Math } from "typescript";

let randomNumber = Math.random();
console.log(randomNumber);
```

Якщо вам потрібно згенерувати випадкове ціле число в певному діапазоні, ви можете скористатися функцією `random()` разом з функціями `floor()` та `ceil()` для округлення числа та `max()` та `min()` для встановлення діапазону.

```TypeScript
let min = 1;
let max = 10;

let randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber);
```

## Глибша аналітика

Коли ми викликаємо функцію `random()`, вона повертає нам випадкове число від 0 до 1. Таке число відоме як псевдовипадкове число, оскільки воно засноване на певному алгоритмі та постійно повторюється. Тому, застосовуючи певні обчислення до цього числа, ми можемо отримати більш складне та випадкове значення.

Також, використовуючи функції `seed()` та `shuffle()`, ми можемо змінити початкове значення алгоритму та отримати ще більше випадковості у наших числах.

## Дивіться також

- [Документація з генерування випадкових чисел](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Практичний приклад реалізації генератора випадкових чисел на TypeScript](https://www.geeksforgeeks.org/how-to-generate-random-number-in-given-range-using-typescript/)
- [Реалізація випадкового розподілу на зразок наступного (Next Method) на TypeScript](https://www.geeksforgeeks.org/generating-random-number-range-javascript/)