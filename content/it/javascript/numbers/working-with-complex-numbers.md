---
date: 2024-01-26 04:42:38.272858-07:00
description: "Come fare: JavaScript non offre un supporto nativo per i numeri complessi,\
  \ ma puoi rimboccarti le maniche e gestirli con oggetti e matematica. Ecco una\u2026"
lastmod: '2024-03-13T22:44:43.805931-06:00'
model: gpt-4-0125-preview
summary: JavaScript non offre un supporto nativo per i numeri complessi, ma puoi rimboccarti
  le maniche e gestirli con oggetti e matematica.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
JavaScript non offre un supporto nativo per i numeri complessi, ma puoi rimboccarti le maniche e gestirli con oggetti e matematica. Ecco una rapida visione.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...aggiungi altri metodi (sottrai, moltiplica, dividi) come necessario

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Risultato: ${result}`); // Risultato: 4 + 6i
```

## Approfondimento
I numeri complessi esistono dal XVI secolo, grazie al matematico italiano Gerolamo Cardano. Sono diventati cruciali in vari campi, come l'ingegneria e la fisica. Nella programmazione moderna, sono chiave per simulazioni e algoritmi che necessitano multidimensionalità.

Ora, JavaScript non è nativamente predisposto per i numeri complessi. Ma oltre all'opzione fai-da-te, potresti utilizzare librerie matematiche come math.js o numeric.js. Offrono la potenza per un lavoro più pesante con i numeri complessi, aggiungendo vantaggi come più operazioni, calcolo della magnitudo e ricerca dell'argomento.

Sotto il cofano, quando operi con numeri complessi, è come gestire due numeri separati legati all'anca. L'addizione e la sottrazione sono giochi diretti—abbina il reale con il reale, l'immaginario con l'immaginario. La moltiplicazione e la divisione diventano piccanti con danze di termini incrociati e richiedono più attenzione.

## Vedi anche
- MDN Web Docs su JavaScript: https://developer.mozilla.org/it/docs/Web/JavaScript/Una_risintroduzione_a_JavaScript
- Math.js, una libreria matematica che include i numeri complessi: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, un'altra libreria: http://numericjs.com/documentation.html
- Un approfondimento sui numeri complessi (focalizzato sulla matematica): https://mathworld.wolfram.com/ComplexNumber.html
