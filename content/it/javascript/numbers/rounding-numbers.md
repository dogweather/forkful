---
date: 2024-01-26 03:45:48.591831-07:00
description: "L'arrotondamento consiste nel tagliare il rumore dopo un certo punto\
  \ in un numero. I programmatori arrotondano per controllare la precisione, gestire\
  \ la\u2026"
lastmod: '2024-03-13T22:44:43.806887-06:00'
model: gpt-4-0125-preview
summary: "L'arrotondamento consiste nel tagliare il rumore dopo un certo punto in\
  \ un numero. I programmatori arrotondano per controllare la precisione, gestire\
  \ la\u2026"
title: Arrotondamento dei numeri
weight: 13
---

## Cosa & Perché?
L'arrotondamento consiste nel tagliare il rumore dopo un certo punto in un numero. I programmatori arrotondano per controllare la precisione, gestire la memoria o rendere l'output user-friendly—come trasformare 2.998 in un netto 3.

## Come fare:
Ecco come arrotondare i numeri in JavaScript usando `Math.round()`, `Math.ceil()` e `Math.floor()`:

```javascript
let numeroOriginale = 2.567;

let arrotondatoVersoIlBasso = Math.floor(numeroOriginale); // 2
let arrotondatoVersoL'Alto = Math.ceil(numeroOriginale);    // 3
let arrotondato = Math.round(numeroOriginale);     // 3 (poiché .567 è maggiore di .5)

console.log(arrotondatoVersoIlBasso); // Stampa: 2
console.log(arrotondatoVersoL'Alto);   // Stampa: 3
console.log(arrotondato);     // Stampa: 3
```

Per fissare un certo numero di decimali, usa `toFixed()`:

```javascript
let dueDecimali = numeroOriginale.toFixed(2); // "2.57" (restituisce una stringa)

console.log(dueDecimali); // Stampa: "2.57"
```

Converti la stringa di nuovo in un numero con un'unario più o `Number()`:

```javascript
let diNuovoNumero = +dueDecimali; // 2.57

console.log(diNuovoNumero); // Stampa: 2.57
```

## Approfondimento
Arrotondare i numeri non è una novità; è vecchio quanto i numeri stessi. In JavaScript, `Math.round()` utilizza il criterio di arrotondamento "arrotonda per eccesso": se la parte frazionaria è 0,5, arrotonda al numero pari più vicino.

Per un controllo maggiore, `toFixed()` potrebbe essere la tua scelta prediletta, ma ricorda, restituisce una stringa. Convertirla di nuovo in un numero potrebbe essere un passaggio in più, ma assicura che continui a lavorare con tipi numerici.

Alternative? Librerie come `lodash` offrono `_.round(number, [precision=0])` per un controllo più sfumato. Oppure, il più recente `Intl.NumberFormat` ti offre formattazioni ad alta precisione oltre il semplice arrotondamento.

Parlando di precisione, attenzione alle peculiarità dei numeri in virgola mobile in JavaScript. `0.1 + 0.2` non è esattamente uguale a `0.3` a causa di come i numeri sono memorizzati. A volte, l'arrotondamento diventa necessario per correggere tali errori di floating-point.

## Vedi Anche
- La documentazione di Math di Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Arrotondamento finanziario con `Intl.NumberFormat`: [API di Internazionalizzazione ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Arrotondamento con `lodash`: [Documentazione di Lodash](https://lodash.com/docs/4.17.15#round)
