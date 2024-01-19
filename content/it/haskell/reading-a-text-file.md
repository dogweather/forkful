---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

La lettura di un file di testo consiste nel recupero di dati memorizzati in un file di testo. I programmatori lo fanno per gestire o analizzare i dati che potrebbero essere contenuti in quei file.

## Come fare:

La funzione principale per leggere un file in Haskell è `readFile`. Di seguito è riportato un esempio di come leggere un file di testo.

```Haskell
import System.IO  
import Control.Exception

main = do  
    contents <- readFile "nomefile.txt"  
    putStr contents 
```

In questo codice, `"nomefile.txt"` deve essere sostituito con il percorso del tuo file di testo. Quando eseguirai il codice, vedrai tutto il contenuto del file di testo stampato nel terminale.

## Un Tuffo Più Profondo

**Contesto storico**: Mentre alcuni linguaggi di programmazione richiedono molteplici passaggi per la lettura di un file di testo, Haskell semplifica il processo a un'operazione di base. Questo rafforza il focus di Haskell su un codice semplice e conciso.

**Alternative**: Altre funzioni in Haskell per gestire i file includono `openFile`, `hGetContents` e `hClose` che offrono una maggiore flessibilità ma anche una maggiore complessità.

**Dettagli implementativi**: `readFile` in Haskell è lazy, il che significa che i contenuti del file vengono letti non appena vengono effettivamente nececessari nel programma. Ciò può essere un vantaggio in termini di efficienza per file molto grandi.

## Vedi Anche

Alcune fonti di riferimento utili per approfondire questi concetti includono:

1. [Informazioni su Haskell e I/O](http://learnyouahaskell.com/input-and-output)
2. [Documentazione della funzione 'readFile'](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:readFile)
3. [Introduzione alla programmazione con Haskell](http://book.realworldhaskell.org/)