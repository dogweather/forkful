---
title:                "Trabajando con números complejos"
date:                  2024-01-26T04:46:13.085499-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con números complejos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Los números complejos, que constan de una parte real y una parte imaginaria (generalmente escritos como a + bi), abren la puerta a cálculos que son prácticamente difíciles o imposibles solo con números reales. Los programadores los utilizan en campos como el procesamiento de señales, la computación cuántica y las matemáticas aplicadas, donde las representaciones numéricas bidimensionales son esenciales.

## Cómo:
Manejar números complejos en TypeScript necesita una clase dedicada. Vamos a crear una y trabajar a través de la adición y la multiplicación.

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

console.log(`Suma: ${sum.toString()}`); // Salida: Suma: 4 + 6i
console.log(`Producto: ${product.toString()}`); // Salida: Producto: -5 + 10i
```

## Análisis Profundo
Históricamente, los números complejos eran controvertidos - incluso se les nombraba como 'imaginarios' para expresar el escepticismo inicial. Ahora, son fundamentales en la matemática y ciencia modernas.

Las alternativas a nuestra clase simple podrían involucrar el uso de bibliotecas existentes como `math.js` o `complex.js`, detalladas con características adicionales como funciones trigonométricas, exponenciación y conjugación compleja.

Los detalles de nuestra implementación en TypeScript se reducen a definir operaciones aritméticas. El método `add` simplemente suma las partes correspondientes. `multiply` aplica el método FOIL utilizado en álgebra, recordando que `i^2 = -1`.

## Ver También
Para más lectura y recursos sobre números complejos y su uso en programación, consulta:

- Álgebra de Números Complejos de MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Biblioteca `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- Biblioteca `complex.js`: https://complex-js.github.io/complex.js/
