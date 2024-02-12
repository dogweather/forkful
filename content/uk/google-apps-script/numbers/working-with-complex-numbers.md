---
title:                "Робота з комплексними числами"
aliases: - /uk/google-apps-script/working-with-complex-numbers.md
date:                  2024-02-01T22:07:35.442542-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Комплексні числа, представлені як комбінація реальної та уявної частин (наприклад, 3 + 4i), є фундаментальними в різноманітних обчислювальних задачах, особливо в інженерії, фізиці та прикладній математиці. Навчання маніпуляціям з цими числами в Google Apps Script дозволяє програмістам розширювати свої можливості в наукових обчисленнях, обробці сигналів та іншому.

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
