---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
La stampa di output di debug in Elm, ovvero `Debug.log`, è un processo utilizzato per monitorare i dati e verificare l'intero comportamento del programma. Questo semplice strumento consente agli sviluppatori di ispezionare valori e tracciare problemi di esecuzione.

## Come fare:
Ecco un esempio di come potresti utilizzare `Debug.log` in Elm:

```Elm
import Debug

add : Int -> Int -> Int
add x y =
  let
    _ = Debug.log "x" x 
    _ = Debug.log "y" y
  in
  x + y

main = 
   Debug.log "The sum is" (add 5 10)
```

L'output prodotto sarà simile a:

```
x: 5
y: 10
The sum is: 15
```

Qui, `Debug.log "x" x` e `Debug.log "y" y` stampano i valori di x e y, mentre `Debug.log "The sum is" (add 5 10)` stampa la somma di questi due numeri.

## Approfondimento
`Debug.log` è un potente strumento per il debug in Elm. Tradizionalmente nel registro degli eventi, gli sviluppatori inseriscono vari messaggi di debug che li aiutano a comprendere come il codice funziona in pratica. In Elm, `Debug.log` offre un mezzo semplice ed efficace per questo. Tuttavia, è importante notare che in un ambiente di produzione, Debug non è disponibile e si dovrebbero eliminare o commentare tutte le chiamate `Debug.log`.

In quanto alle alternative, `Debug.todo` può essere utilizzato quando c'è bisogno di un promemoria su qualcosa che deve essere implementato in futuro. Oppure, è possibile utilizzare `Debug.toString` per convertire qualsiasi valore in una stringa che può essere stampata.

Il funzionamento di `Debug.log` è piuttosto semplice: prende due argomenti, una stringa (il tag del log) e un valore di qualsiasi tipo, infine mostra il tag e il valore nell'output della console.

## Vedi anche:
1. Documentazione ufficiale di Elm - Debug: https://package.elm-lang.org/packages/elm/core/latest/Debug
2. Un articolo utile sul debug in Elm: https://www.elm-tutorial.org/en-v01/02-elm-arch/07-debugging.html
3. Guide per Elm - Debugging: https://guide.elm-lang.org/effects/debugging.html