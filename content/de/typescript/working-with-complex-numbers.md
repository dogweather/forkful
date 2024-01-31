---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:46:08.755653-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen, bestehend aus einem Realteil und einem Imaginärteil (üblicherweise geschrieben als a + bi), ermöglichen Berechnungen, die mit nur reellen Zahlen praktisch unmöglich oder nicht durchführbar wären. Programmierer nutzen sie in Bereichen wie der Signalverarbeitung, Quantencomputing und angewandten Mathematik, wo zweidimensionale Zahlenrepräsentationen essentiell sind.

## Wie:
Die Handhabung komplexer Zahlen in TypeScript erfordert eine dedizierte Klasse. Lassen Sie uns eine erstellen und uns mit Addition und Multiplikation auseinandersetzen.

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

console.log(`Summe: ${sum.toString()}`); // Ausgabe: Summe: 4 + 6i
console.log(`Produkt: ${product.toString()}`); // Ausgabe: Produkt: -5 + 10i
```

## Vertiefung
Historisch gesehen waren komplexe Zahlen umstritten - sogar als 'imaginär' geprägt, um die anfängliche Skepsis auszudrücken. Jetzt sind sie grundlegend in der modernen Mathematik und Wissenschaft.

Alternativen zu unserer einfachen Klasse könnten die Verwendung bestehender Bibliotheken wie `math.js` oder `complex.js` einschließen, die mit zusätzlichen Funktionen wie trigonometrischen Funktionen, Exponentiation und Komplexkonjugation ausgestattet sind.

Die Details unserer TypeScript-Implementierung kommen auf die Definition von arithmetischen Operationen herunter. Die Methode `add` addiert einfach die entsprechenden Teile. `multiply` wendet die FOIL-Methode an, die in der Algebra verwendet wird, wobei man sich daran erinnert, dass `i^2 = -1` ist.

## Siehe auch
Für weiterführende Literatur und Ressourcen zu komplexen Zahlen und ihrer Verwendung in der Programmierung, siehe:

- MDN Komplexe Zahl Algebra: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` Bibliothek: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` Bibliothek: https://complex-js.github.io/complex.js/
