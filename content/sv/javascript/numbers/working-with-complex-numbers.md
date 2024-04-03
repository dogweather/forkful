---
date: 2024-01-26 04:42:27.788166-07:00
description: "Hur man g\xF6r: JavaScript har inte inbyggt st\xF6d f\xF6r komplexa\
  \ tal, men du kan kavla upp \xE4rmarna och hantera det med objekt och matematik.\
  \ H\xE4r \xE4r en\u2026"
lastmod: '2024-03-13T22:44:38.287437-06:00'
model: gpt-4-0125-preview
summary: "JavaScript har inte inbyggt st\xF6d f\xF6r komplexa tal, men du kan kavla\
  \ upp \xE4rmarna och hantera det med objekt och matematik."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur man gör:
JavaScript har inte inbyggt stöd för komplexa tal, men du kan kavla upp ärmarna och hantera det med objekt och matematik. Här är en snabbgenomgång.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...lägg till fler metoder (subtrahera, multiplicera, dela) efter behov

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const resultat = a.add(b);

console.log(`Resultat: ${resultat}`); // Resultat: 4 + 6i
```

## Fördjupning
Komplexa tal har funnits sedan 1500-talet, tack vare den italienska matematikern Gerolamo Cardano. De har blivit avgörande inom olika områden, som ingenjörsvetenskap och fysik. I modern programmering är de nyckeln för simuleringar och algoritmer som behöver multidimensionalitet.

Nu är inte JavaScript laddat för komplexa tal från början. Men förutom DIY-alternativet, kan du använda matematikbibliotek som math.js eller numeric.js. De har kraften för tyngre lyft med komplexa tal, och lägger till förmåner som fler operationer, beräkning av magnitud och argumenthitta.

Under huven, när du opererar med komplexa tal, är det som att hantera två separata tal bundna vid höften. Addition och subtraktion är enkla spel - matcha det reella med det reella, det imaginära med det imaginära. Multiplikation och division blir kryddigare med korsvis interaktion och kräver mer omsorg.

## Se även
- MDN Web Docs om JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, ett matematikbibliotek inklusive komplexa tal: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, ett annat bibliotek: http://numericjs.com/documentation.html
- En fördjupning om komplexa tal (matematikfokuserat): https://mathworld.wolfram.com/ComplexNumber.html
