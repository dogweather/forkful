---
date: 2024-01-26 04:46:53.251589-07:00
description: "\u042F\u043A: \u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438\
  \ \u0437 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u043C\u0438\
  \ \u0447\u0438\u0441\u043B\u0430\u043C\u0438 \u0443 TypeScript \u043F\u043E\u0442\
  \u0440\u0456\u0431\u0435\u043D \u0441\u043F\u0435\u0446\u0456\u0430\u043B\u044C\u043D\
  \u0438\u0439 \u043A\u043B\u0430\u0441. \u0414\u0430\u0432\u0430\u0439\u0442\u0435\
  \ \u0441\u0442\u0432\u043E\u0440\u0438\u043C\u043E \u043E\u0434\u0438\u043D \u0442\
  \u0430 \u043F\u0440\u043E\u0439\u0434\u0435\u043C\u043E\u0441\u044F \u043F\u043E\
  \ \u0434\u043E\u0434\u0430\u0432\u0430\u043D\u043D\u044E \u0456 \u043C\u043D\u043E\
  \u0436\u0435\u043D\u043D\u044E."
lastmod: '2024-03-13T22:44:48.858936-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u043A\u043E\
  \u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\
  \u0430\u043C\u0438 \u0443 TypeScript \u043F\u043E\u0442\u0440\u0456\u0431\u0435\u043D\
  \ \u0441\u043F\u0435\u0446\u0456\u0430\u043B\u044C\u043D\u0438\u0439 \u043A\u043B\
  \u0430\u0441."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

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
