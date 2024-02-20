---
date: 2024-01-26 04:42:55.096681-07:00
description: "Komplexe Zahlen sind Zahlen mit einem Real- und einem Imagin\xE4rteil\
  \ (wie 3 + 4i). Sie tauchen in verschiedenen Programmierproblemen auf, insbesondere\
  \ in\u2026"
lastmod: 2024-02-19 22:05:13.197333
model: gpt-4-0125-preview
summary: "Komplexe Zahlen sind Zahlen mit einem Real- und einem Imagin\xE4rteil (wie\
  \ 3 + 4i). Sie tauchen in verschiedenen Programmierproblemen auf, insbesondere in\u2026"
title: Umgang mit komplexen Zahlen
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen sind Zahlen mit einem Real- und einem Imaginärteil (wie 3 + 4i). Sie tauchen in verschiedenen Programmierproblemen auf, insbesondere in der Signalverarbeitung, Quantencomputing und bei der Lösung von Polynomgleichungen. Programmierer jonglieren mit ihnen, um diese Art von Aufgaben effektiv zu bewältigen.

## Wie geht das:
JavaScript hat keine eingebaute Unterstützung für komplexe Zahlen, aber man kann die Ärmel hochkrempeln und es mit Objekten und Mathematik bewerkstelligen. Hier ist ein schneller Überblick.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...füge weitere Methoden hinzu (subtrahiere, multipliziere, teile), wie benötigt

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Ergebnis: ${result}`); // Ergebnis: 4 + 6i
```

## Tiefere Einblicke
Komplexe Zahlen gibt es seit dem 16. Jahrhundert, dank des italienischen Mathematikers Gerolamo Cardano. Sie wurden in verschiedenen Bereichen wie Ingenieurwesen und Physik wesentlich. In der modernen Programmierung sind sie Schlüssel für Simulationen und Algorithmen, die Mehrdimensionalität benötigen.

Nun, JavaScript ist nativ nicht für komplexe Zahlen ausgestattet. Aber neben der DIY-Option könnte man Mathematikbibliotheken wie math.js oder numeric.js verwenden. Sie bieten die Kraft für schwereres Heben von komplexen Zahlen, mit Vorteilen wie mehr Operationen, Magnitudenberechnung und Argumentfindung.

Unter der Haube, wenn Sie mit komplexen Zahlen arbeiten, ist es wie das Verwalten von zwei getrennten Zahlen, die an der Hüfte gebunden sind. Addition und Subtraktion sind direkte Spiele – passe das Reale mit dem Realen, das Imaginäre mit dem Imaginären an. Multiplikation und Division werden würzig mit Überkreuzungen und benötigen mehr Sorgfalt.

## Siehe auch
- MDN Web Docs zu JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, eine Mathematikbibliothek, die komplexe Zahlen einschließt: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, eine weitere Bibliothek: http://numericjs.com/documentation.html
- Ein tieferer Einblick in komplexe Zahlen (mathematisch fokussiert): https://mathworld.wolfram.com/ComplexNumber.html
