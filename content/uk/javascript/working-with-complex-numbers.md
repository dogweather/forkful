---
title:                "Робота з комплексними числами"
date:                  2024-01-26T04:43:23.882326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що та Чому?
Комплексні числа – це числа з реальною та уявною частинами (наприклад, 3 + 4i). Вони зустрічаються в різноманітних програмних завданнях, особливо при обробці сигналів, квантових обчисленнях та розв'язанні поліноміальних рівнянь. Програмісти маніпулюють ними для ефективного вирішення цих завдань.

## Як:
JavaScript не має вбудованої підтримки комплексних чисел, але ви можете звернутися до об'єктів та математики, щоб з цим впоратися. Ось швидкий приклад.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...додайте більше методів (віднімання, множення, ділення) за потребою

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Результат: ${result}`); // Результат: 4 + 6i
```

## Поглиблене вивчення
Комплексні числа існують з 16 століття завдяки італійському математику Героламо Кардано. Вони стали невід'ємною частиною в різних галузях, таких як інженерія та фізика. У сучасному програмуванні вони ключові для симуляцій та алгоритмів, що потребують багатовимірності.

Тепер, JavaScript не має нативної підтримки комплексних чисел. Але, крім DIY варіанту, ви можете використовувати математичні бібліотеки, такі як math.js або numeric.js. Вони пропонують потужні можливості для роботи з комплексними числами, додаючи переваги, як більше операцій, розрахунок магнітуди та визначення аргументу.

Внутрішньо, коли ви працюєте з комплексними числами, це ніби керування двома окремими числами, які зв'язані між собою. Додавання та віднімання є простими операціями – зіставляєте реальні з реальними, уявні з уявними. Множення та ділення стають цікавішими з танцювальними перехрестями термінів і потребують більше уваги.

## Дивіться також
- MDN Web Docs про JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, математична бібліотека, що включає комплексні числа: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, інша бібліотека: http://numericjs.com/documentation.html
- Поглиблене вивчення комплексних чисел (орієнтоване на математику): https://mathworld.wolfram.com/ComplexNumber.html
