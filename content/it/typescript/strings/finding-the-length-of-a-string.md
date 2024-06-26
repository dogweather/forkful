---
date: 2024-01-20 17:48:38.175242-07:00
description: "How to: (Come fare:) Semlice, no? TypeScript ti fornisce la propriet\xE0\
  \ `.length` direttamente sulle stringhe."
lastmod: '2024-04-05T22:40:30.328261-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Semlice, no?
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to: (Come fare:)
```TypeScript
let saluto: string = "Ciao mondo!";
let lunghezza: number = saluto.length;

console.log(lunghezza); // Output: 12
```
Semlice, no? TypeScript ti fornisce la proprietà `.length` direttamente sulle stringhe.

```TypeScript
function stampaLunghezza(str: string) {
    console.log(`La lunghezza della stringa è: ${str.length}`);
}

stampaLunghezza("Buongiorno"); // Output: La lunghezza della stringa è: 10
stampaLunghezza(""); // Output: La lunghezza della stringa è: 0
```
Funziona anche con stringhe vuote!

## Deep Dive (Approfondimento)
La proprietà `.length` è ereditata da `String.prototype` in JavaScript (e quindi in TypeScript). Dall'inizio del JavaScript, la lunghezza delle stringhe è stata fondamentale per operazioni di base come cicli e confronti.

### Storico
Nei primi giorni, i linguaggi programmatici spesso lavoravano con array di caratteri piuttosto che con oggetti stringa. Ogni linguaggio gestiva la lunghezza in maniera diversa, alcuni con funzioni dedicate e altri con convenzioni (ad es., il carattere terminatore nulla in C).

### Alternative
In TypeScript, `.length` è il modo standard e più diretto, ma per esperimento, potresti usare codice più verboso come:

```TypeScript
let lunghezzaManuale = Array.from(saluto).length;
console.log(lunghezzaManuale); // Output: 12
```
Questo converte la stringa in un array di caratteri e poi conta gli elementi. Utile per stringhe con caratteri Unicode speciali che `.length` potrebbe non valutare correttamente.

### Implementazione
In TypeScript, il conteggio è basato sul numero di unità di codice UTF-16, quindi i caratteri rappresentati come surrogate pairs (spesso emoji o caratteri non latini) verranno contati come due anziché uno.

## See Also (Vedi Anche)
- Documentazione ufficiale di TypeScript sulla tipizzazione delle stringhe: [String Type](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- Un articolo approfondito sui caratteri Unicode e JavaScript/TypeScript: [Unicode in JavaScript](https://mathiasbynens.be/notes/javascript-unicode)
