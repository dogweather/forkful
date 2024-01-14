---
title:    "Haskell: Scrivere all'errore standard"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Scrivere in Haskell è una pratica comune tra i programmatori funzionali. Una delle azioni più comuni che si possono eseguire durante lo sviluppo in Haskell è la scrittura sullo standard error. Ma perché dovremmo farlo?

Scrivere su standard error può essere utile in situazioni in cui si vogliono visualizzare informazioni di debug, errori o messaggi di avviso. Questo ci permette di tenere separati i messaggi di debug dai normali output del programma.

## Come fare

Per scrivere su standard error in Haskell, possiamo usare la funzione `hPutStrLn` dal modulo `System.IO`. Ecco un esempio di codice che stampa un messaggio di debug sulla console di errore:

```Haskell
import System.IO

hPutStrLn stderr "Questo è un messaggio di debug."
```

L'output sarà:

```
Questo è un messaggio di debug.
```

In questo modo possiamo visualizzare informazioni importanti sullo stato del nostro programma senza dover interrompere il flusso normale di output.

## Approfondimento

Scrivere su standard error può essere utile anche in situazioni in cui si vuole gestire gli errori in modo diverso dal solito. Per esempio, possiamo utilizzare la funzione `hPutStrLn` per scrivere gli errori su un file di log anziché sulla console di errore.

Inoltre, è possibile gestire i messaggi di errore in modo più preciso utilizzando la funzione `hPutStr` anziché `hPutStrLn`. In questo modo, possiamo stampare solo una parte del messaggio sugli errori anziché tutto il testo.

## Vedi anche

- [Documentazione di Haskell su System.IO](https://www.haskell.org/onlinereport/stdlibrary.html#filesystem)

- [Tutorial su Come usare Haskell con file di testo](https://www.tutorialspoint.com/haskell/haskell_files_io.htm)

- [Guida su Gestione degli errori in Haskell](https://mmhaskell.com/blog/2017/9/22/handling-errors-in-haskell)