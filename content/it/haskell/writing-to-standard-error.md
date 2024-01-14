---
title:    "Haskell: Scrivere su errore standard"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un'abilità fondamentale per qualsiasi programmatore Haskell. Ci permette di visualizzare informazioni importanti durante l'esecuzione del codice e di risolvere problemi di debugging in modo più efficace.

## Come fare

Per scrivere su standard error in Haskell, segue questi semplici passaggi:

  1. Importa il modulo `System.IO` nella sezione `import` del tuo codice.
  ```
  import System.IO
  ```
  2. Usa la funzione `hPutStrLn` per scrivere una stringa su standard error.
  ```
  hPutStrLn stderr "Questo è un messaggio di errore."
  ```
  3. Ricorda di compilare il tuo codice usando il flag `-threaded` per evitare problemi di buffering.

## Deep Dive

La funzione `hPutStrLn` accetta due argomenti: il primo è il canale di output su cui scrivere (in questo caso `stderr` per standard error), mentre il secondo è la stringa che vogliamo scrivere. Se si desidera scrivere più di una riga su standard error, è possibile utilizzare la funzione `hPutStr` per scrivere una stringa senza un carattere di nuova linea alla fine.

Inoltre, è possibile sostituire `stderr` con `stdout` per scrivere sulla standard output invece che standard error.

## Vedi anche

- [Haskell Documentation on System.IO](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.14.0.0/System-IO.html)
- [Writing to Standard Error in Haskell](https://rosettacode.org/wiki/Writing_to_standard_error#Haskell)