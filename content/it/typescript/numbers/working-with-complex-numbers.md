---
date: 2024-01-26 04:46:21.085325-07:00
description: 'Come Fare: Gestire i numeri complessi in TypeScript richiede una classe
  dedicata. Creiamone una e procediamo con l''addizione e la moltiplicazione.'
lastmod: '2024-03-13T22:44:43.169940-06:00'
model: gpt-4-0125-preview
summary: Gestire i numeri complessi in TypeScript richiede una classe dedicata.
title: Lavorare con i numeri complessi
weight: 14
---

## Come Fare:
Gestire i numeri complessi in TypeScript richiede una classe dedicata. Creiamone una e procediamo con l'addizione e la moltiplicazione.

```TypeScript
class Complesso {
    constructor(public re: number, public im: number) {}

    add(other: Complesso): Complesso {
        return new Complesso(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complesso): Complesso {
        return new Complesso(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complesso(1, 2);
let num2 = new Complesso(3, 4);
let somma = num1.add(num2);
let prodotto = num1.multiply(num2);

console.log(`Somma: ${somma.toString()}`); // Output: Somma: 4 + 6i
console.log(`Prodotto: ${prodotto.toString()}`); // Output: Prodotto: -5 + 10i
```

## Approfondimento
Storicamente, i numeri complessi erano controversi - addirittura denominati 'immaginari' per esprimere lo scetticismo iniziale. Ora, sono fondamentali nella matematica e nella scienza moderna.

Alternative alla nostra semplice classe potrebbero coinvolgere l'uso di librerie esistenti come `math.js` o `complex.js`, arricchite con funzionalità aggiuntive come le funzioni trigonometriche, l'esponenziazione e la coniugazione complessa.

La nostra implementazione in TypeScript si riduce alla definizione delle operazioni aritmetiche. Il metodo `add` si limita a sommare le parti corrispondenti. `multiply` applica il metodo FOIL usato in algebra, ricordando che `i^2 = -1`.

## Vedi Anche
Per ulteriori letture e risorse sui numeri complessi e il loro uso nella programmazione, consulta:

- Algebra dei Numeri Complessi MDN: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- libreria `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- libreria `complex.js`: https://complex-js.github.io/complex.js/
