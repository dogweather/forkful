---
title:                "Работа с комплексными числами"
date:                  2024-01-29T00:06:01.941634-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с комплексными числами"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/working-with-complex-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Комплексные числа, состоящие из действительной и мнимой части (обычно записываются как a + bi), позволяют выполнять вычисления, которые были бы непрактичными или невозможными, используя только действительные числа. Программисты используют их в таких областях, как обработка сигналов, квантовые вычисления и прикладная математика, где необходимо двумерное представление чисел.

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
