---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:35.442542-07:00
description: "\u042F\u043A: Google Apps Script \u043D\u0435 \u043C\u0430\u0454 \u0432\
  \u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\
  \u0438\u043C\u043A\u0438 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\
  \u0445 \u0447\u0438\u0441\u0435\u043B, \u0449\u043E \u043F\u043E\u0442\u0440\u0435\
  \u0431\u0443\u0454 \u0440\u0435\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457\
  \ \u0432\u043B\u0430\u0441\u043D\u043E\u0433\u043E \u0444\u0443\u043D\u043A\u0446\
  \u0456\u043E\u043D\u0430\u043B\u0443. \u041D\u0438\u0436\u0447\u0435 \u043D\u0430\
  \u0432\u0435\u0434\u0435\u043D\u0430 \u0431\u0430\u0437\u043E\u0432\u0430 \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0430 \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:48.499635-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\
  \u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\
  \u0438 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u0445 \u0447\
  \u0438\u0441\u0435\u043B, \u0449\u043E \u043F\u043E\u0442\u0440\u0435\u0431\u0443\
  \u0454 \u0440\u0435\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u0457 \u0432\u043B\
  \u0430\u0441\u043D\u043E\u0433\u043E \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\
  \u0430\u043B\u0443."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Як:
Google Apps Script не має вбудованої підтримки комплексних чисел, що потребує реалізації власного функціоналу. Нижче наведена базова структура для роботи з комплексними числами, включаючи додавання, віднімання та множення.

```javascript
// Визначаємо конструктор для комплексних чисел
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Метод для додавання двох комплексних чисел
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Метод для віднімання двох комплексних чисел
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Метод для множення двох комплексних чисел
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Приклад використання
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Додавання двох комплексних чисел
var sum = num1.add(num2);
console.log(`Сума: ${sum.real} + ${sum.imag}i`); // Сума: 4 + 6i

// Віднімання двох комплексних чисел
var difference = num1.subtract(num2);
console.log(`Різниця: ${difference.real} + ${difference.imag}i`); // Різниця: 2 + 2i

// Множення двох комплексних чисел
var product = num1.multiply(num2);
console.log(`Добуток: ${product.real} + ${product.imag}i`); // Добуток: -5 + 10i
```

## Глибоке занурення:
Концепція комплексних чисел сягає 16 століття, але роботи математиків таких як Ейлер і Гаусс закріпили їх місце в математиці. Незважаючи на їх корисність, комплексні числа не мають прямої підтримки в JavaScript або, за розширенням, в Google Apps Script. Відсутність нативної підтримки означає, що операції з комплексними числами доводиться реалізовувати вручну, як було продемонстровано. Хоча це забезпечує хороші можливості для навчання і достатню функціональність для базових потреб, для важких обчислювальних робіт, що вимагають комплексних чисел, можна замислитися про використання інших програмувальних середовищ, більш придатних для математичних обчислень, таких як Python з NumPy, які пропонують вбудовані, високооптимізовані операції для роботи з комплексними числами. Тим не менш, розуміння та впровадження базових операцій в Google Apps Script є корисним вправою для тих, хто прагне розширити свої програмувальні навички і застосувати їх у широкому спектрі контекстів.
