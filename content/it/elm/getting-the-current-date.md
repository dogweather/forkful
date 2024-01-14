---
title:                "Elm: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un elemento essenziale in molti programmi. Potresti aver bisogno di utilizzarla per gestire eventi, mostrare notifiche o semplicemente per aggiornare informazioni in tempo reale.

## Come fare

Per ottenere la data corrente in Elm, dobbiamo utilizzare il modulo `Time`. Per prima cosa, dobbiamo importare questo modulo:

```Elm
import Time exposing (..)
```

Successivamente, possiamo utilizzare la funzione `now` per ottenere la data corrente. Questa funzione ci restituirà un valore di tipo `Task Time.Posix` che dobbiamo elaborare utilizzando la funzione `andThen`:

```Elm
now
    |> andThen (always currentData)
```

Dove `currentDate` è una funzione che elabora il valore di `Time.Posix` per ottenere la data formattata in modo leggibile.

## Approfondimento

Il valore restituito dalla funzione `now` è di tipo `Task Time.Posix` perché la data corrente potrebbe non essere immediatamente disponibile. Questo perché viene utilizzata la funzione `performance.now` del browser, che restituisce il tempo trascorso dall'avvio del browser in millisecondi.

Inoltre, il modulo `Time` ci offre diverse funzioni per manipolare i valori di tempo, come ad esempio `add`, che ci permette di aggiungere un certo numero di millisecondi a una data.

## Vedi anche

- Documentazione del modulo `Time`: https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial di Elm sulla gestione del tempo: https://www.elm-tutorial.org/en/06-the-elm-architecture/05-time.html
- Esempi di utilizzo della data corrente in Elm: https://github.com/search?p=1&q=elm+current+date&type=Repositories