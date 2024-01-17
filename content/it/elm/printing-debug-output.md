---
title:                "Stampa dell'output di debug"
html_title:           "Elm: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Stampare l'output di debug è una tecnica utilizzata dai programmatori per visualizzare informazioni aggiuntive durante l'esecuzione di un programma. Questo può aiutare a identificare ed eliminare eventuali errori o problemi.

## Come fare:
Ecco un esempio di come stampare output di debug in Elm utilizzando la funzione `Debug.log`:
```
import Debug exposing (log)

-- Definizione di una funzione che somma due numeri
add : Int -> Int -> Int
add x y =
  Debug.log "Somma" (x + y)
```
Output:
```
Somma 7
```

## Approfondimento:
La pratica di stampare l'output di debug è stata utilizzata fin dai primi tempi della programmazione, quando i programmatori dovevano dipendere da output su carta o su console per verificare il funzionamento del loro codice. Oggi, ci sono anche altre tecniche di debugging, come l'utilizzo di debugger e test, ma l'output di debug può ancora essere utile per identificare errori in modo rapido ed efficiente.

## Vedi anche:
Per ulteriori informazioni sui metodi di debugging in Elm, puoi consultare la documentazione ufficiale su [Debugging](https://guide.elm-lang.org/debugging/).