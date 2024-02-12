---
title:                "Werken met complexe getallen"
aliases: - /nl/typescript/working-with-complex-numbers.md
date:                  2024-01-28T22:12:57.796549-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, bestaande uit een reëel deel en een imaginair deel (meestal geschreven als a + bi), maken berekeningen die onpraktisch of onmogelijk zijn met alleen reële getallen mogelijk. Programmeurs gebruiken ze op gebieden zoals signaalverwerking, kwantumcomputing en toegepaste wiskunde, waar tweedimensionale getallenweergaven essentieel zijn.

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
