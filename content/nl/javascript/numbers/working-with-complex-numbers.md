---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:14.187683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen zijn getallen met een reëel en een imaginair deel (zoals 3 + 4i). Ze komen voor in verschillende programmeerproblemen, met name in signaalverwerking, quantumcomputing en het oplossen van polynomiale vergelijkingen. Programmeurs jongleren ermee om dit soort taken effectief aan te pakken.

## Hoe:
JavaScript heeft geen ingebouwde ondersteuning voor complexe getallen, maar je kunt de mouwen opstropen en het aanpakken met objecten en wiskunde. Hier is een snelle handleiding.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...voeg meer methoden toe (aftrekken, vermenigvuldigen, delen) naar behoefte

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const resultaat = a.add(b);

console.log(`Resultaat: ${resultaat}`); // Resultaat: 4 + 6i
```

## Uitdieping
Complexe getallen bestaan al sinds de 16e eeuw, dankzij de Italiaanse wiskundige Gerolamo Cardano. Ze werden cruciaal in verschillende velden, zoals techniek en natuurkunde. In moderne programmering zijn ze essentieel voor simulaties en algoritmen die meerdimensionaliteit nodig hebben.

Nu, JavaScript is van nature niet uitgerust voor complexe getallen. Maar naast de doe-het-zelf optie, kun je wiskundige bibliotheken gebruiken zoals math.js of numeric.js. Ze bieden de kracht voor het zwaardere werk met complexe getallen, met extra's zoals meer operaties, het berekenen van grootte, en het vinden van argumenten.

Onder de motorkap is werken met complexe getallen alsof je twee aparte getallen beheert die aan elkaar vastzitten. Optellen en aftrekken zijn eenvoudige handelingen—match het reële met het reële, het imaginaire met het imaginaire. Vermenigvuldigen en delen worden pittiger met kruistermen dansen en vergen meer zorg.

## Zie ook
- MDN Web Docs over JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, een wiskundebibliotheek inclusief complexe getallen: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, nog een bibliotheek: http://numericjs.com/documentation.html
- Een diepere duik in complexe getallen (wiskundig gericht): https://mathworld.wolfram.com/ComplexNumber.html