---
title:                "Elm: Scrivere su standard error"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrive a standard error può essere utile quando si vuole gestire gli errori in modo specifico all'interno del proprio programma Elm.

## Come Fare

Per scrivere a standard error in Elm, basta utilizzare la funzione `Debug.crash` passando come argomento una stringa di testo.

```Elm
import Debug exposing (crash)

main =
  crash "Errore personalizzato"
```

L'esempio sopra produrrà una riga di testo nell'output del browser che può essere utilizzata per individuare la posizione di un errore durante lo sviluppo.

Output: `-- ERROR ------------------------------------------------------- Errore personalizzato ------------------------------------------------------`

## Approfondimento

La funzione `Debug.crash` è utile per debugging e sviluppo, ma non dovrebbe essere utilizzata nella produzione finale in quanto interrompe l'esecuzione del programma. Invece, è consigliato utilizzare la gestione standard degli errori di Elm, come la funzione `Result.withDefault` per gestire i possibili errori all'interno del tuo programma.

## Vedi Anche

- [La guida ufficiale di Elm per la gestione degli errori](https://guide.elm-lang.org/error_handling/)
- [Il repository ufficiale di Elm su Github](https://github.com/elm/)