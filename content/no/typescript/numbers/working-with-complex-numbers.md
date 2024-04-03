---
date: 2024-01-26 04:46:23.798045-07:00
description: "Komplekse tall, som best\xE5r av en reell del og en imagin\xE6r del\
  \ (vanligvis skrevet som a + bi), \xE5pner for beregninger som er upraktiske eller\
  \ umulige med\u2026"
lastmod: '2024-03-13T22:44:40.526071-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall, som best\xE5r av en reell del og en imagin\xE6r del (vanligvis\
  \ skrevet som a + bi), \xE5pner for beregninger som er upraktiske eller umulige\
  \ med bare reelle tall."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Å håndtere komplekse tall i TypeScript krever en dedikert klasse. La oss lage en og jobbe gjennom addisjon og multiplikasjon.

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
let produkt = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // Utgang: Sum: 4 + 6i
console.log(`Produkt: ${produkt.toString()}`); // Utgang: Produkt: -5 + 10i
```

## Dypdykk
Historisk sett var komplekse tall kontroversielle - til og med omtalt som 'imaginære' for å uttrykke opprinnelig skepsis. Nå er de grunnleggende i moderne matematikk og vitenskap.

Alternativer til vår enkle klasse kan innebære å bruke eksisterende biblioteker som `math.js` eller `complex.js`, detaljert med ytterligere funksjoner som trigonometriske funksjoner, eksponentiering og kompleks konjugasjon.

Vår TypeScript-implementering detaljer går ned til å definere aritmetiske operasjoner. `add`-metoden legger ganske enkelt til tilsvarende deler. `multiply` anvender FOIL-metoden som brukes i algebra, med å huske at `i^2 = -1`.

## Se Også
For videre lesing og ressurser om komplekse tall og deres bruk i programmering, sjekk ut:

- MDN Kompleks Tall Algebra: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` bibliotek: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` bibliotek: https://complex-js.github.io/complex.js/
