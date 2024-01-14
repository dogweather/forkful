---
title:                "Elm: Calcolo della lunghezza di una stringa"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché 
Ci sono molte ragioni per cui riporre tempo ed energia nella programmazione in Elm, una delle quali è la sua semplicità e la sua capacità di gestire stringhe in modo efficiente. Trovare la lunghezza di una stringa è un'operazione comune nella programmazione e in questo articolo ti mostrerò come farlo in Elm.

## Come fare
Per trovare la lunghezza di una stringa in Elm, puoi utilizzare la funzione `String.length` che prende una stringa come input e restituisce il numero di caratteri all'interno di quella stringa. Vediamo un esempio di codice:

```Elm
let
  str = "Ciao a tutti"
  length = String.length str
in
  length -- output: 12
```
In questo esempio, abbiamo dichiarato una variabile `str` che contiene la stringa "Ciao a tutti" e poi abbiamo utilizzato la funzione `String.length` per trovare la sua lunghezza. Infine, abbiamo assegnato il risultato alla variabile `length` e l'abbiamo visualizzato come output.

## Approfondimento
Oltre alla funzione `String.length`, ci sono altre alternative per trovare la lunghezza di una stringa in Elm. Una di queste è utilizzare il metodo `List.length` che prende una lista di caratteri come input e restituisce il numero di elementi all'interno della lista, che corrispondono alla lunghezza della stringa. Allo stesso modo, esiste anche il metodo `Array.length` che funziona con gli array.

Vediamo un esempio di codice utilizzando questi metodi:

```Elm
let
  str = "Ciao a tutti"
  charList = String.toList str
  length = List.length charList
  array = String.toArray str
  length2 = Array.length array
in
  length -- output: 12
  length2 -- output: 12
```

Come puoi vedere, abbiamo prima convertito la stringa in una lista di caratteri utilizzando il metodo `String.toList` e poi abbiamo utilizzato il metodo `List.length` per trovare la sua lunghezza. Lo stesso discorso vale per gli array, dove abbiamo utilizzato il metodo `String.toArray` per convertire la stringa in un array di caratteri e poi abbiamo utilizzato il metodo `Array.length` per trovare la lunghezza.

## Vedi anche
- [Documentazione di Elm sulle stringhe](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial di Elm: Introduzione alle stringhe](https://guide.elm-lang.org/strings/)