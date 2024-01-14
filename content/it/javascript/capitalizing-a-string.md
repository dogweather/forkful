---
title:                "Javascript: Capitalizzare una stringa"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché 

Capitalizzare una stringa è un'operazione comune nella programmazione, in particolare quando si lavora con stringhe di testo. Capitalizzare una stringa significa trasformare tutte le lettere in maiuscolo. Questo può essere utile per scopi di formattazione o di confronto di stringhe.

## Come Fare

Per capitalizzare una stringa in Javascript, è possibile utilizzare il metodo `.toUpperCase()` che è disponibile per ogni stringa. Questo metodo restituirà una nuova stringa con tutte le lettere in maiuscolo. Ecco un esempio di come utilizzare questo metodo:

```Javascript
let stringa = "ciao mondo";
let stringaCapitalizzata = stringa.toUpperCase();

console.log(stringaCapitalizzata); // STAMPA: CIAO MONDO
```

In questo esempio, abbiamo dichiarato una variabile `stringa` con un valore di "ciao mondo". Poi, abbiamo utilizzato il metodo `.toUpperCase()` per trasformare la stringa in maiuscolo e assegnare il risultato alla variabile `stringaCapitalizzata`. Infine, abbiamo stampato il risultato utilizzando `console.log`.

Se si vuole capitalizzare solo la prima lettera di una stringa, è possibile utilizzare il metodo `.charAt()` per ottenere la prima lettera e poi concatenarla con la stringa in maiuscolo. Ecco un esempio:

```Javascript
let stringa = "ciao mondo";
let primaLettera = stringa.charAt(0); // ottiene la prima lettera "c"
let restoStringa = stringa.slice(1); // ottiene il resto della stringa "iao mondo"
let stringaCapitalizzata = primaLettera.toUpperCase() + restoStringa; // concatena la prima lettera maiuscola con il resto della stringa

console.log(stringaCapitalizzata); // STAMPA: Ciao mondo
```

## Approfondimento 

Oltre ai metodi presentati sopra, esistono altre tecniche per capitalizzare una stringa in Javascript. Ad esempio, è possibile usare una funzione ricorsiva che verifica ogni lettera della stringa e la trasforma in maiuscolo. O ancora, si può utilizzare la libreria lodash che ha una funzione specifica per capitalizzare stringhe.

Anche se può sembrare un'operazione semplice, capitalizzare una stringa richiede una buona comprensione della manipolazione di stringhe e dei metodi disponibili in Javascript. Inoltre, è importante tenere conto delle differenze tra maiuscole e minuscole nelle stringhe in base all'uso che se ne vuole fare.

## Vedi Anche 

- [Maggiori informazioni sul metodo `.toUpperCase()` in Javascript](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Esempi di stringhe capitalizzate in Javascript](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [La libreria lodash per la manipolazione di stringhe in Javascript] (https://lodash.com/docs/4.17.15#capitalize)