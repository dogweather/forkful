---
title:                "Trovare la lunghezza di una stringa"
aliases:
- /it/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:38.175242-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Calcolare la lunghezza di una stringa significa contare quanti caratteri contiene. I programmatori lo fanno per validazioni, limitazioni di input e manipolazioni di testo.

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
