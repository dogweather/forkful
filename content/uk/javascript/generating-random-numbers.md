---
title:                "Створення випадкових чисел"
html_title:           "Javascript: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Генерація випадкових чисел є важливою частиною програмування, особливо в Язиці Javascript. Вона дозволяє нам створювати різноманітні програми, які можуть представляти випадковий вибір, поведінку або результати.

## Як

Генерація випадкових чисел у Javascript доволі проста. За допомогою вбудованої функції `Math.random()`, ми можемо отримати випадкове число в діапазоні від 0 до 1. Також, щоб наші числа були цілочисельними, ми можемо використати функцію `Math.floor()` для заокруглення десяткових чисел.

```Javascript
// Генеруємо випадкове число від 0 до 10
let randomNumber = Math.random() * 10;
// Отримуємо ціле число
let integer = Math.floor(randomNumber);
```

Ми також можемо використовувати випадкові числа для створення рядка символів, використовуючи функцію `String.fromCharCode()`. Наприклад, ми можемо створити випадковий пароль, який буде складатися з випадкових літер латинського алфавіту.

```Javascript
// Генеруємо випадкове число від 97 до 122 (код ASCII латинських букв)
let randomChar = Math.floor(Math.random() * (122 - 97 + 1) + 97);
// Конвертуємо число в символ
let char = String.fromCharCode(randomChar);
// Додаємо символ до рядка
let password = char;
// Повторюємо цей процес для отримання більш довгого пароля
for (let i = 0; i < length; i++) {
  randomChar = Math.floor(Math.random() * (122 - 97 + 1) + 97);
  char = String.fromCharCode(randomChar);
  password += char;
}

console.log(password); // приклад випадкового пароля "nhgkjhgct"
```

## Deep Dive

В своїй основі, у випадкових чисел в Язиці Javascript є генерація псевдовипадкових чисел за допомогою алгоритму. Цей алгоритм базується на початковому значенні, яке називається "насінням". Кожного разу, коли ми викликаємо функцію `Math.random()`, насіння підсумовується зі значенням 134775813 і ділиться на шлях початкового насіння. Це дозволяє отримувати різні випадкові числа при кожному виклику.

## See Also

- [Документація за випадковими числами в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Стаття про генерацію псевдовипадкових чисел у JavaScript](https://www.geeksforgeeks.org/javascript-math-random-function/)