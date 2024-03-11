---
date: 2024-01-26 04:46:21.085325-07:00
description: "I numeri complessi, composti da una parte reale e una immaginaria (solitamente\
  \ scritti come a + bi), rendono possibili calcoli impraticabili o impossibili\u2026"
lastmod: '2024-03-11T00:14:16.731665-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi, composti da una parte reale e una immaginaria (solitamente\
  \ scritti come a + bi), rendono possibili calcoli impraticabili o impossibili\u2026"
title: Lavorare con i numeri complessi
---

{{< edit_this_page >}}

## Cos'è e Perché?
I numeri complessi, composti da una parte reale e una immaginaria (solitamente scritti come a + bi), rendono possibili calcoli impraticabili o impossibili con i soli numeri reali. I programmatori li utilizzano in campi come l'elaborazione dei segnali, il calcolo quantistico e la matematica applicata, dove le rappresentazioni numeriche bidimensionali sono essenziali.

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
