---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та навіщо?
Генерація випадкових чисел - це процес створення чисел, які не можна передбачити краще, ніж випадковим вибором, і вони мають широке застосування у програмуванні. Розробники часто використовують випадкові числа для тестування програм, в іграх, криптографії, моделюванні та інших областях.

## Як це робити:
JavaScript надає вбудовані функції для генерування випадкових чисел. Ось декілька прикладів:

```Javascript
// Генерує випадкове число від 0 (включно) до 1 (не включаючи)
let random = Math.random();
console.log(random);
```

```Javascript
// Генерує випадкове ціле число від 1 до 100
let randomInt = Math.floor(Math.random() * 100) + 1;
console.log(randomInt);
```

Ці випадкові числа можуть вар'юватися від 0 до 99 (або 0 до 0.999999 в першому випадку).

## Поглиблений аналіз
### Історичний контекст
Насправді, ідея про те, що числа можуть бути "випадковими" і "непередбачуваними", існувала ще до епохи комп'ютерів.

### Альтернативи
Math.random() - це ваш стандартний інструмент, але маються і інші опції. Наприклад, можна використовувати web API, як window.crypto.getRandomValues.

### Деталі реалізації
Math.random() - це псевдовипадковий генератор чисел. Це означає, що, хоча його вихід виглядає випадковим, він все-таки генерується з певного сів зразка і, теоретично, може бути відтворений.

## Див. також:
- [MDN Web Docs: Math.random()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs: window.crypto.getRandomValues](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [Understanding Math.random()](https://medium.com/@oldwestaction/randomness-is-hard-e085decbcbb2)