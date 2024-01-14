---
title:    "Gleam: Trova la lunghezza di una stringa."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione che ci permette di manipolare e gestire dati in modo efficace. In questo articolo esploreremo come trovare la lunghezza di una stringa utilizzando il linguaggio di programmazione Gleam.

## Come fare

Per trovare la lunghezza di una stringa in Gleam, possiamo utilizzare la funzione `length` che fa parte della libreria incorporata `String`. Dobbiamo solo passare la stringa come parametro e la funzione restituirà il numero di caratteri all'interno della stringa.

Prendiamo ad esempio questa stringa: `"Ciao amici!"`. Per trovarne la lunghezza, possiamo scrivere il seguente codice all'interno di un blocco di codice ```Gleam
import String
let a_string = "Ciao amici!"
let length = String.length(a_string)
```

L'output di questo codice sarà `12`, poiché ci sono 12 caratteri all'interno della stringa, compresi gli spazi vuoti. Possiamo anche utilizzare questa funzione per controllare se una stringa ha una determinata lunghezza o per confrontare la lunghezza di due stringhe diverse.

## Approfondimento

La funzione `length` in realtà utilizza l'algoritmo "conta e ripeti" per trovare la lunghezza della stringa. In poche parole, l'algoritmo scorre attraverso ogni carattere della stringa e conta quanti ce ne sono, restituendo il numero totale.

È importante ricordare che, poiché Gleam è un linguaggio fortemente tipizzato, la funzione `length` restituirà solo la lunghezza di una stringa e non può essere utilizzata per trovare la lunghezza di altri tipi di dati come liste o tuple.

## Guarda anche

- [Documentazione ufficiale per la libreria String di Gleam](https://gleam.run/modules/stdlib/String.html)
- [Ulteriori informazioni sull'algoritmo "conta e ripeti"](https://www.geeksforgeeks.org/how-to-find-length-of-a-string-using-iteration-in-python/)