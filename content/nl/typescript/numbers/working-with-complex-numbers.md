---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:57.796549-07:00
description: "Hoe te: Om complexe getallen in TypeScript te hanteren, is een toegewijde\
  \ klasse nodig. Laten we er een maken en door de optelling en vermenigvuldiging\u2026"
lastmod: '2024-03-13T22:44:50.543933-06:00'
model: gpt-4-0125-preview
summary: Om complexe getallen in TypeScript te hanteren, is een toegewijde klasse
  nodig.
title: Werken met complexe getallen
weight: 14
---

## Hoe te:
Om complexe getallen in TypeScript te hanteren, is een toegewijde klasse nodig. Laten we er een maken en door de optelling en vermenigvuldiging heen werken.

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

console.log(`Som: ${sum.toString()}`); // Uitvoer: Som: 4 + 6i
console.log(`Product: ${product.toString()}`); // Uitvoer: Product: -5 + 10i
```

## Diepgaande Verkenning
Historisch gezien waren complexe getallen controversieel - zelfs bestempeld als 'imaginair' om de initiële scepsis uit te drukken. Nu zijn ze fundamenteel in moderne wiskunde en wetenschap.

Alternatieven voor onze eenvoudige klasse kunnen het gebruik van bestaande bibliotheken zoals `math.js` of `complex.js` omvatten, gedetailleerd met extra functies zoals trigonometrische functies, exponentiatie en complex conjugatie.

Onze TypeScript-implementatie details komen neer op het definiëren van rekenkundige bewerkingen. De `add` methode voegt simpelweg overeenkomstige delen toe. `multiply` past de FOIL methode toe die wordt gebruikt in algebra, onthoudend dat `i^2 = -1`.

## Zie Ook
Voor verder lezen en bronnen over complexe getallen en hun gebruik in programmeren, bekijk:

- MDN Complexe Getal Algebra: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` bibliotheek: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` bibliotheek: https://complex-js.github.io/complex.js/
