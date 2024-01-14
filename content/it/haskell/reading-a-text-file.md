---
title:                "Haskell: Lettura di un file di testo"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché leggere un file di testo

Leggere un file di testo è una delle attività più comuni quando si lavora con programmi di elaborazione dei dati. Può essere utile per analizzare grandi quantità di informazioni o per accedere rapidamente ai dati strutturati all'interno di un file.

## Come fare

In Haskell, esistono diversi modi per leggere un file di testo. Il seguente esempio mostra come leggere il contenuto di un file utilizzando la funzione `readFile` e stampare il suo output su schermo:

```Haskell
main = do
  contents <- readFile "esempio.txt"
  putStrLn contents
```

L'output sarà il contenuto del file di testo stampato su schermo, nel formato di una stringa.

```
Questo è un esempio di testo
utilizzato per illustrare
come leggere un file in Haskell.
```

## Approfondimento

La funzione `readFile` è molto utile, ma può causare problemi di prestazioni quando si lavora con file di grandi dimensioni. Per evitare questo, è possibile utilizzare la funzione `openFile` insieme alla funzione `hGetContents`. Questo permette di accedere ai dati in modo più efficiente, in particolare quando si lavora con file di grandi dimensioni.

Per leggere un file utilizzando questa metodologia, è necessario aprire il file, ottenere un handle e quindi utilizzare la funzione `hGetContents` per recuperare i dati. L'handle viene quindi chiuso utilizzando la funzione `hClose`. Il seguente esempio mostra come leggere un file utilizzando questa metodologia:

```Haskell
main = do
  handle <- openFile "esempio.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle 
```

È importante ricordare di chiudere l'handle dopo averlo utilizzato per evitare perdite di memoria.

## Vedi anche

- [Funzione `readFile` nel modulo `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:readFile)
- [Funzione `openFile` nel modulo `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:openFile)
- [Funzione `hGetContents` nel modulo `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hGetContents)
- [Funzione `hClose` nel modulo `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hClose)