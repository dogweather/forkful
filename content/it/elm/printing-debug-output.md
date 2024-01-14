---
title:                "Elm: Stampa dell'output di debug"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante lo sviluppo di un progetto Elm, ci troviamo di fronte a bug o a comportamenti inaspettati. In questi casi è fondamentale avere un modo per visualizzare informazioni sullo stato del nostro programma e sulla logica che lo sta guidando. Ecco perché la stampa di output di debug può essere uno strumento molto utile.

## Come fare

Per stampare output di debug in Elm, possiamo utilizzare la funzione `Debug.log` presente nel modulo `Debug`. Possiamo passare a questa funzione una stringa descrittiva del nostro output di debug e una variabile che vogliamo visualizzare. Ad esempio:

```Elm
import Debug exposing (log)

x = 5
log "Valore di x" x

...

--------------------------------------
Valore di x: 5
```

Kpossiamo anche passare più variabili alla funzione, separandole con una virgola. Inoltre, possiamo utilizzare la combinazione di stringhe e variabili per creare output di debug più elaborati. Ad esempio:

```Elm
import Debug exposing (log)

nome = "Mario"
cognome = "Rossi"
log "Nome completo" (nome ++ " " ++ cognome)

...

--------------------------------------
Nome completo: Mario Rossi
```

## Approfondimento

Il modulo `Debug` offre molte altre funzioni oltre a `log`, come ad esempio `crash` per generare un messaggio di errore, `todo` per creare un punto di implementazione mancante e `program` per mostrare lo stato attuale del nostro programma. Inoltre, possiamo utilizzare la funzione `Debug.watch` per monitorare una variabile e visualizzarne il valore in tempo reale.

## Vedi anche

- Documentazione ufficiale sul modulo `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Utilizzo della funzione Debug.watch: https://www.elm-tutorial.org/debugging/01.html
- Tutorial sulla stampa di output di debug in Elm: https://www.elm-tutorial.org/debugging/