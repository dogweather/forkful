---
title:                "Lettura degli argomenti sulla riga di comando"
html_title:           "Haskell: Lettura degli argomenti sulla riga di comando"
simple_title:         "Lettura degli argomenti sulla riga di comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
 Leggere gli argomenti della riga di comando è una pratica comune tra i programmatori per ottenere informazioni dall'utente tramite il terminale. È utile quando si vuole rendere il programma più interattivo o personalizzabile per l'utente.

## Come fare:
```Haskell
-- Utilizzando il modulo System.Environment
import System.Environment (getArgs)

-- Funzione per stampare gli argomenti ricevuti
printArgs :: [String] -> IO ()
printArgs args = putStrLn ("Hai fornito gli argomenti: " ++ unwords args)

-- Ottenere e stampare gli argomenti
main :: IO ()
main = do
  args <- getArgs
  printArgs args
```
Questo codice importa il modulo `System.Environment` che ci permette di utilizzare la funzione `getArgs` per ottenere una lista di `String`, ovvero gli argomenti della riga di comando passati al programma. La funzione `printArgs` prende questa lista come parametro e la stampa a video concatenando gli elementi con uno spazio. Nella funzione `main`, utilizzando il costrutto `do`, otteniamo gli argomenti dalla riga di comando tramite `getArgs` e li passiamo alla funzione `printArgs`.

Esempio di output:

```
$ runhaskell args.hs arg1 arg2 arg3
Hai fornito gli argomenti: arg1 arg2 arg3
```

## Approfondimento:
La lettura degli argomenti della riga di comando è una funzionalità disponibile in molti linguaggi di programmazione, ma in Haskell è particolarmente semplice grazie alla sua forte tipizzazione e alla gestione delle funzioni come dati. Un'alternativa a `getArgs` è `getProgName` che restituisce il nome del programma invece degli argomenti. Ci sono anche librerie esterne come `System.Console.ArgParser` che forniscono una sintassi più flessibile per la gestione degli argomenti.

## Vedi anche:
- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial interattivo su Haskell](https://www.haskell.org/try/)
- [Modulo System.Environment di Haskell](https://hackage.haskell.org/package/base/docs/System-Environment.html)