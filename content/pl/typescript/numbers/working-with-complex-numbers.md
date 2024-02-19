---
aliases:
- /pl/typescript/working-with-complex-numbers/
date: 2024-01-26 04:46:27.960189-07:00
description: "Liczby zespolone, sk\u0142adaj\u0105ce si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ oraz cz\u0119\u015Bci urojonej (zwykle zapisywane jako a + bi), umo\u017Cliwiaj\u0105\
  \ obliczenia, kt\xF3re by\u0142yby\u2026"
lastmod: 2024-02-18 23:08:49.342202
model: gpt-4-0125-preview
summary: "Liczby zespolone, sk\u0142adaj\u0105ce si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ oraz cz\u0119\u015Bci urojonej (zwykle zapisywane jako a + bi), umo\u017Cliwiaj\u0105\
  \ obliczenia, kt\xF3re by\u0142yby\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone, składające się z części rzeczywistej oraz części urojonej (zwykle zapisywane jako a + bi), umożliwiają obliczenia, które byłyby niepraktyczne lub niemożliwe tylko przy użyciu liczb rzeczywistych. Programiści wykorzystują je w dziedzinach takich jak przetwarzanie sygnałów, informatyka kwantowa i matematyka stosowana, gdzie niezbędne są dwuwymiarowe reprezentacje liczb.

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
