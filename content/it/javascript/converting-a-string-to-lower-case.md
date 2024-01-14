---
title:    "Javascript: Conversione di una stringa in lettere minuscole"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché

In programmazione, spesso ci troviamo ad avere a che fare con stringhe di testo. Per una maggiore flessibilità e facilità di manipolazione, è importante che siano tutte formattate allo stesso modo. Convertire una stringa in minuscolo è un modo semplice per ottenere un output uniforme e coerente.

## Come fare

```Javascript
const stringa = "CIAO A TUTTI"
console.log(stringa.toLowerCase())
```
Output: "ciao a tutti"

Utilizzando il metodo `.toLowerCase()` su una variabile di tipo stringa, questa verrà convertita interamente in minuscolo. Questo metodo è utile quando si vuole confrontare o cercare una stringa senza dover preoccuparsi delle differenze tra lettere maiuscole e minuscole.

Un altro modo per ottenere lo stesso risultato è utilizzando il metodo `.replace()` insieme alla regex `/[A-Z]/g` come parametro per sostituire tutte le lettere maiuscole con le corrispondenti lettere minuscole.
```Javascript
const stringa = "CIao a Tutti"
console.log(stringa.replace(/[A-Z]/g, function(lettera) {
  return lettera.toLowerCase();
}))
```
Output: "ciao a tutti"

## Immergiamoci nel dettaglio

La conversione di una stringa in minuscolo può essere eseguita utilizzando anche il metodo `.toLocaleLowerCase()` che tiene conto delle regole specifiche della lingua di cui la stringa fa parte. Inoltre, è importante ricordare che la conversione avviene solo per le lettere, mentre i numeri, i simboli e gli spazi rimangono invariati.

È anche possibile utilizzare la libreria esterna `lodash` che offre il metodo `.lowerCase()` per convertire una stringa in minuscolo.

## Vedi anche

- [MDN: String.prototype.toLowerCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [MDN: String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [MDN: RegExp](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Lodash: _.lowerCase()](https://lodash.com/docs/4.17.15#lowerCase)