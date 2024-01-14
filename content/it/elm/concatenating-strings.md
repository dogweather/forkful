---
title:    "Elm: Concatenazione di stringhe"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione fondamentale nella programmazione di Elm. È importante perché ci permette di unire due o più stringhe insieme per creare una nuova stringa più lunga. Questo è particolarmente utile quando dobbiamo creare messaggi di errore dinamici o visualizzare dati all'interno di una stringa.

## Come Fare

Per concatenare le stringhe in Elm, possiamo utilizzare l'operatore `++` o la funzione `String.concat`.

```Elm
nome = "Maria"
cognome = "Rossi"

saluto = "Ciao " ++ nome ++ " " ++ cognome

-- Output: "Ciao Maria Rossi"

lista = ["Uno", "Due", "Tre"]

numeri = String.concat lista

-- Output: "UnoDueTre"
```

Come possiamo vedere dagli esempi sopra, possiamo unire stringhe utilizzando l'operatore `++` o passando una lista di stringhe alla funzione `String.concat`.

## Approfondimento

Esistono alcune considerazioni da tenere presente quando si concatenano le stringhe in Elm. Innanzitutto, è importante che tutte le stringhe siano dello stesso tipo. Ciò significa che non è possibile unire una stringa con un intero o un float. Se vogliamo convertire un valore in una stringa, possiamo utilizzare la funzione `toString`.

```Elm
numero = 123
stringa = "Il numero è " ++ toString numero

-- Output: "Il numero è 123"
```

Inoltre, quando si concatenano più stringhe, è importante prestare attenzione alle prestazioni. Se dobbiamo unire molte stringhe insieme, è consigliabile utilizzare la funzione `String.concat` invece dell'operatore `++`, poiché la prima è più efficiente in termini di prestazioni.

## Vedi Anche

- [Documentazione su come manipolare le stringhe in Elm](https://guide.elm-lang.org/effects/string.html)
- [Esempi pratici sull'utilizzo del concatenamento delle stringhe in Elm](https://www.elmbasics.com/concatenating-strings)