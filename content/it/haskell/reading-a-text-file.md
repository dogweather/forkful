---
title:                "Lettura di un file di testo"
html_title:           "Haskell: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

La lettura di un file di testo è semplicemente il processo di estrarre il contenuto di un file di testo dal disco rigido e visualizzarlo sullo schermo o utilizzarlo per elaborazioni ulteriori. I programmatori spesso leggono i file di testo per accedere a dati salvati in questo formato o per analizzare e manipolare il contenuto del file.

## Come Fare:

Un modo comune per leggere un file di testo in Haskell è utilizzare la funzione `readFile` dal modulo `System.IO`. Questa funzione prende come argomento il percorso del file da leggere e restituisce il contenuto del file come una stringa. Ad esempio, se abbiamo un file di testo chiamato "dati.txt" contenente "Ciao Mondo!", possiamo leggerlo nel seguente modo:

```Haskell
contenutoFile <- readFile "dati.txt"
putStrLn contenutoFile
```

Questo codice stamperebbe "Ciao Mondo!" sulla console. Per elaborazioni più complesse, è possibile utilizzare la funzione `lines` per suddividere il contenuto del file in righe e manipolarle separatamente.

## Approfondimento:

La lettura di file di testo è un'operazione fondamentale per la programmazione e viene utilizzata in molte situazioni. In passato, i dati venivano spesso memorizzati in file di testo poiché era più facile e veloce modificarli a mano rispetto ai file binari. Tuttavia, oggigiorno ci sono molte alternative, come i database, che offrono un'elaborazione più efficiente e sicura dei dati. La lettura di file di testo può anche essere soggetta a errori, ad esempio se il formato del file è errato o se il file non esiste.

## Vedi Anche:

- [Haskell I/O](https://www.haskell.org/tutorial/stdio.html)
- [Funzione `readFile`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:readFile)
- [Funzione `lines`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:lines)