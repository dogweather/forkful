---
date: 2024-01-26 04:46:27.960189-07:00
description: "Jak to zrobi\u0107: Obs\u0142uga liczb zespolonych w TypeScript wymaga\
  \ dedykowanej klasy. Stw\xF3rzmy jedn\u0105 i prze\u0107wiczymy dodawanie oraz mno\u017C\
  enie."
lastmod: '2024-03-13T22:44:35.132850-06:00'
model: gpt-4-0125-preview
summary: "Obs\u0142uga liczb zespolonych w TypeScript wymaga dedykowanej klasy."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
Obsługa liczb zespolonych w TypeScript wymaga dedykowanej klasy. Stwórzmy jedną i przećwiczymy dodawanie oraz mnożenie.

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
let suma = num1.add(num2);
let iloczyn = num1.multiply(num2);

console.log(`Suma: ${suma.toString()}`); // Wynik: Suma: 4 + 6i
console.log(`Iloczyn: ${iloczyn.toString()}`); // Wynik: Iloczyn: -5 + 10i
```

## Dogłębna analiza
Historycznie liczby zespolone były kontrowersyjne - nawet określane jako "urojone", aby wyrazić początkowy sceptycyzm. Obecnie są one fundamentalne w nowoczesnej matematyce i nauce.

Alternatywy dla naszej prostej klasy mogłyby obejmować użycie istniejących bibliotek takich jak `math.js` lub `complex.js`, bogatych w dodatkowe funkcje takie jak funkcje trygonometryczne, potęgowanie i sprzężenie zespolone.

Nasza implementacja w TypeScript sprowadza się do definiowania operacji arytmetycznych. Metoda `add` po prostu dodaje odpowiadające sobie części. `multiply` stosuje metodę FOIL używaną w algebrze, pamiętając, że `i^2 = -1`.

## Zobacz także
Aby uzyskać więcej informacji i zasobów na temat liczb zespolonych i ich zastosowania w programowaniu, sprawdź:

- Algebra liczb zespolonych na MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- Biblioteka `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- Biblioteka `complex.js`: https://complex-js.github.io/complex.js/
