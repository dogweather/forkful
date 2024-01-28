---
title:                "Робота з комплексними числами"
date:                  2024-01-26T04:46:53.251589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?
Комплексні числа, що складаються з реальної та уявної частин (зазвичай записують як a + bi), розкривають можливості для розрахунків, непрактичних або неможливих з використанням лише дійсних чисел. Програмісти використовують їх у таких галузях, як обробка сигналів, квантові обчислення та застосована математика, де двомірне представлення чисел є важливим.

## Як:
Для роботи з комплексними числами у TypeScript потрібен спеціальний клас. Давайте створимо один та пройдемося по додаванню і множенню.

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

console.log(`Sum: ${sum.toString()}`); // Вивід: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // Вивід: Product: -5 + 10i
```

## Поглиблений розгляд
Історично, комплексні числа були предметом суперечок - навіть отримали назву "уявні", щоб виразити первісний скептицизм. Зараз вони є фундаментальними в сучасній математиці та науці.

Альтернативи нашому простому класу можуть включати використання існуючих бібліотек, як-от `math.js` або `complex.js`, деталізовані додатковими функціями, такими як тригонометричні функції, експоненціювання та комплексне спряження.

Деталі нашої реалізації на TypeScript зводяться до визначення арифметичних операцій. Метод `add` просто додає відповідні частини. `multiply` застосовує метод FOIL, що використовується в алгебрі, пам'ятаючи, що `i^2 = -1`.

## Дивіться також
Для подальшого читання та ресурсів про комплексні числа та їх використання в програмуванні, перегляньте:

- МДН Алгебра комплексних чисел: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- бібліотека `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- бібліотека `complex.js`: https://complex-js.github.io/complex.js/
