---
date: 2024-01-25 02:59:56.973374-07:00
description: 'How to: Handling complex numbers in TypeScript needs a dedicated class.
  Let''s create one and work through addition and multiplication.'
lastmod: '2024-03-13T22:44:59.852842-06:00'
model: gpt-4-1106-preview
summary: Handling complex numbers in TypeScript needs a dedicated class.
title: Working with complex numbers
weight: 14
---

## How to:
Handling complex numbers in TypeScript needs a dedicated class. Let's create one and work through addition and multiplication.

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

console.log(`Sum: ${sum.toString()}`); // Output: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // Output: Product: -5 + 10i
```

## Deep Dive
Historically, complex numbers were controversial - even coined as 'imaginary' to express initial skepticism. Now, they're foundational in modern mathematics and science. 

Alternatives to our simple class might involve using existing libraries such as `math.js` or `complex.js`, detailed with additional features like trigonometric functions, exponentiation, and complex conjugation.

Our TypeScript implementation details boil down to defining arithmetic operations. The `add` method simply adds corresponding parts. `multiply` applies the FOIL method used in algebra, remembering that `i^2 = -1`.

## See Also
For further reading and resources on complex numbers and their use in programming, check out:

- MDN Complex Number Algebra: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` library: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` library: https://complex-js.github.io/complex.js/
