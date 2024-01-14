---
title:    "TypeScript: Trova la lunghezza di una stringa"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è una delle operazioni più comuni quando si lavora con dati di testo. Sapere come farlo in TypeScript ti aiuterà a gestire meglio le stringhe nei tuoi progetti e a scrivere codice più efficace.

## Come fare

In TypeScript, per trovare la lunghezza di una stringa, possiamo utilizzare il metodo `length` che è disponibile su tutte le istanze di `string`. Vediamo un esempio:

```TypeScript
let testo = "Questo è un esempio";
console.log(testo.length); // Output: 18
```

Come puoi vedere, il valore ottenuto è corretto, poiché conta anche gli spazi vuoti tra le parole. Possiamo anche utilizzare questa funzione in combinazione con altre, come `slice` per estrarre una parte della stringa e quindi trovare la lunghezza di quella sottostringa. Vediamo un altro esempio:

```TypeScript
let testo = "Questo è un esempio";
let sottostringa = testo.slice(9, 15);
console.log(sottostringa.length); // Output: 6
```

In questo caso, il valore ottenuto è 6, poiché abbiamo estratto una parte della stringa composta da 6 caratteri.

## Approfondimento

Oltre al metodo `length`, TypeScript ci offre altre opzioni per manipolare e ottenere informazioni sulla lunghezza delle stringhe. Possiamo utilizzare ad esempio il concetto di *array-like* per trattare una stringa come un array di caratteri e quindi utilizzare metodi come `join` o `reverse`. Inoltre, possiamo anche utilizzare regExp per cercare specifici caratteri o parole all'interno di una stringa e ottenere la loro lunghezza.

## Vedi anche

- [Documentazione ufficiale di TypeScript sulle stringhe](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Tutorial su come manipolare le stringhe in TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Espressioni regolari in TypeScript](https://medium.com/@basarat/typescript-string-only-regular-expression-2183f4a85256)