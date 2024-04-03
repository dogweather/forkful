---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:01.941634-07:00
description: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430, \u0441\u043E\u0441\u0442\u043E\u044F\u0449\u0438\u0435\
  \ \u0438\u0437 \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043B\u044C\
  \u043D\u043E\u0439 \u0438 \u043C\u043D\u0438\u043C\u043E\u0439 \u0447\u0430\u0441\
  \u0442\u0438 (\u043E\u0431\u044B\u0447\u043D\u043E \u0437\u0430\u043F\u0438\u0441\
  \u044B\u0432\u0430\u044E\u0442\u0441\u044F \u043A\u0430\u043A a + bi), \u043F\u043E\
  \u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u0432\u044B\u043F\u043E\u043B\u043D\u044F\
  \u0442\u044C \u0432\u044B\u0447\u0438\u0441\u043B\u0435\u043D\u0438\u044F, \u043A\
  \u043E\u0442\u043E\u0440\u044B\u0435 \u0431\u044B\u043B\u0438 \u0431\u044B\u2026"
lastmod: '2024-03-13T22:44:44.578057-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430, \u0441\u043E\u0441\u0442\u043E\u044F\u0449\u0438\u0435\
  \ \u0438\u0437 \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043B\u044C\
  \u043D\u043E\u0439 \u0438 \u043C\u043D\u0438\u043C\u043E\u0439 \u0447\u0430\u0441\
  \u0442\u0438 (\u043E\u0431\u044B\u0447\u043D\u043E \u0437\u0430\u043F\u0438\u0441\
  \u044B\u0432\u0430\u044E\u0442\u0441\u044F \u043A\u0430\u043A a + bi), \u043F\u043E\
  \u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u0432\u044B\u043F\u043E\u043B\u043D\u044F\
  \u0442\u044C \u0432\u044B\u0447\u0438\u0441\u043B\u0435\u043D\u0438\u044F, \u043A\
  \u043E\u0442\u043E\u0440\u044B\u0435 \u0431\u044B\u043B\u0438 \u0431\u044B \u043D\
  \u0435\u043F\u0440\u0430\u043A\u0442\u0438\u0447\u043D\u044B\u043C\u0438 \u0438\u043B\
  \u0438 \u043D\u0435\u0432\u043E\u0437\u043C\u043E\u0436\u043D\u044B\u043C\u0438\
  , \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u0442\u043E\u043B\u044C\
  \u043A\u043E \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043B\u044C\u043D\
  \u044B\u0435 \u0447\u0438\u0441\u043B\u0430."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как:
Для работы с комплексными числами в TypeScript необходим специализированный класс. Давайте создадим такой и рассмотрим операции сложения и умножения.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Сумма: ${sum.toString()}`); // Вывод: Сумма: 4 + 6i
console.log(`Произведение: ${product.toString()}`); // Вывод: Произведение: -5 + 10i
```

## Глубокое погружение
Исторически комплексные числа были предметом споров - даже получили название "мнимые", чтобы выразить первоначальный скептицизм. Теперь они являются основополагающими в современной математике и науке.

Альтернативы нашему простому классу могут включать использование существующих библиотек, таких как `math.js` или `complex.js`, с дополнительными функциями, такими как тригонометрические функции, возведение в степень и комплексное сопряжение.

Детали нашей реализации на TypeScript сводятся к определению арифметических операций. Метод `add` просто складывает соответствующие части. Метод `multiply` применяет метод FOIL, используемый в алгебре, помня, что `i^2 = -1`.

## Смотри также
Для дальнейшего чтения и ресурсов о комплексных числах и их использовании в программировании, смотрите:

- Алгебра комплексных чисел на MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Библиотека `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- Библиотека `complex.js`: https://complex-js.github.io/complex.js/
