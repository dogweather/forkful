---
date: 2024-01-26 04:42:54.264007-07:00
description: "Komplekse tall er tall med en reel og en imagin\xE6r del (som 3 + 4i).\
  \ De dukker opp i ulike programmeringsproblemer, spesielt i signalbehandling,\u2026"
lastmod: '2024-03-13T22:44:41.178074-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall er tall med en reel og en imagin\xE6r del (som 3 + 4i)."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
JavaScript har ikke innebygd støtte for komplekse tall, men du kan ta saken i egne hender og håndtere det med objekter og matte. Her er en kjapp gjennomgang.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...legg til flere metoder (trekk fra, multipliser, del) etter behov

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const resultat = a.add(b);

console.log(`Resultat: ${resultat}`); // Resultat: 4 + 6i
```

## Dypdykk
Komplekse tall har vært rundt siden det 16. århundre, takket være den italienske matematikeren Gerolamo Cardano. De ble avgjørende i ulike felter, som ingeniørvitenskap og fysikk. I moderne programmering er de nøkkelen for simuleringer og algoritmer som trenger multidimensjonalitet.

Nå er ikke JavaScript lastet for komplekse tall naturligvis. Men ved siden av DIY-alternativet, kan du bruke mattebiblioteker som math.js eller numeric.js. De tilbyr kraften for tyngre løft av komplekse tall, og tilføyer fordeler som flere operasjoner, beregning av størrelse, og argumentfinning.

Under panseret, når du opererer med komplekse tall, er det som å håndtere to separate tall bundet ved hoften. Addisjon og subtraksjon er rett fram – match det reelle med det reelle, det imaginære med det imaginære. Multiplikasjon og divisjon blir mer spennende med kryssvilkår-danser og trenger mer forsiktighet.

## Se også
- MDN Web Docs om JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, et mattebibliotek inkludert komplekse tall: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, et annet bibliotek: http://numericjs.com/documentation.html
- Et dypere dykk på komplekse tall (mattefokusert): https://mathworld.wolfram.com/ComplexNumber.html
